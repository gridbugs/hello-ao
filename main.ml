module Sync_channel = struct
  type channel =
    { mutable send_state : int64
    ; mutable reciever_state : int64
    ; mutex : Mutex.t
    ; send_and_recv_states_are_different : Condition.t
    }

  module Send = struct
    type nonrec t = { channel : channel }

    let send { channel } =
      Mutex.lock channel.mutex;
      channel.send_state <- Int64.add channel.send_state Int64.one;
      Condition.signal channel.send_and_recv_states_are_different;
      Mutex.unlock channel.mutex
    ;;
  end

  module Recv = struct
    type nonrec t = { channel : channel }

    let recv { channel } =
      Mutex.lock channel.mutex;
      while Int64.equal channel.send_state channel.reciever_state do
        Condition.wait channel.send_and_recv_states_are_different channel.mutex
      done;
      channel.reciever_state <- channel.send_state;
      Mutex.unlock channel.mutex
    ;;
  end

  (* A communication channel between a pair of threads: the "sender" and
     "reciever". The reciever can call [recv] to wait until the sender calls
     [send]. The [recv] function will block until [send] is called, unless
     [send] has been called since the last time [recv] returned. *)
  type t =
    { send : Send.t
    ; recv : Recv.t
    }

  (* Create a linked receiver/sender pair. *)
  let create () =
    let channel =
      { send_state = Int64.zero
      ; reciever_state = Int64.zero
      ; mutex = Mutex.create ()
      ; send_and_recv_states_are_different = Condition.create ()
      }
    in
    { send = { Send.channel }; recv = { Recv.channel } }
  ;;
end

module Viz_queue = struct
  module Writer = struct
    type 'a t = { queue : 'a Queue.t }

    let push t x = Queue.push x t.queue
  end

  type 'a t =
    { queue : 'a Queue.t
    ; mutex : Mutex.t
    }

  let create () = { queue = Queue.create (); mutex = Mutex.create () }

  let drain t ~f =
    Mutex.protect t.mutex (fun () ->
      let rec loop () =
        match Queue.take_opt t.queue with
        | Some x ->
          f x;
          loop ()
        | None -> ()
      in
      loop ())
  ;;

  let with_writer t ~f = Mutex.protect t.mutex (fun () -> f { Writer.queue = t.queue })
end

module Signal = struct
  module Ctx = struct
    type t = { sample_rate_hz : float }
  end

  type t = Ctx.t -> float

  let sample (t : t) ctx = t ctx

  module Arith = struct
    let ( + ) a b ctx = a ctx +. b ctx
    let ( * ) a b ctx = a ctx *. b ctx
  end
end

module Stereo_signal = struct
  type t =
    { left : Signal.t
    ; right : Signal.t
    }

  let make ~left ~right = { left; right }
  let sample t ctx = Signal.sample t.left ctx, Signal.sample t.right ctx
end

module Player = struct
  let bits = 16
  let rate = 44100
  let channels = 2

  type t =
    { device : Ao.t
    ; byte_format : Ao.byte_format_t
    }

  module Double_buffer = struct
    type t =
      { mutable generate_into : Bytes.t
      ; mutable play_from : Bytes.t
      }

    let create size = { generate_into = Bytes.create size; play_from = Bytes.create size }

    let swap t =
      let tmp = t.generate_into in
      t.generate_into <- t.play_from;
      t.play_from <- tmp
    ;;
  end

  let create () =
    let driver = Ao.get_default_driver () in
    let byte_format = Ao.driver_preferred_byte_format driver in
    let device = Ao.open_live ~bits ~rate ~channels ~driver ~byte_format () in
    { device; byte_format }
  ;;

  let close t = Ao.close t.device

  let sample_to_byte_pair_lo_hi sample =
    (* Clamp the sample between -1 and 1. *)
    let sample_clamped = Float.min sample 1.0 |> Float.max (-1.0) in
    (* Convert to a signed int between -32767 and 32767.*)
    let sample_int = int_of_float (sample_clamped *. 32767.0) in
    let byte_lo = sample_int land 0xFF |> char_of_int in
    let byte_hi = (sample_int lsr 8) land 0xFF |> char_of_int in
    byte_lo, byte_hi
  ;;

  let write_bytes_to_buffer_le buffer offset ~left_lo ~left_hi ~right_lo ~right_hi =
    Bytes.set buffer offset left_lo;
    Bytes.set buffer (offset + 1) left_hi;
    Bytes.set buffer (offset + 2) right_lo;
    Bytes.set buffer (offset + 3) right_hi
  ;;

  let write_bytes_to_buffer_be buffer offset ~left_lo ~left_hi ~right_lo ~right_hi =
    Bytes.set buffer offset left_hi;
    Bytes.set buffer (offset + 1) left_lo;
    Bytes.set buffer (offset + 2) right_hi;
    Bytes.set buffer (offset + 3) right_lo
  ;;

  let run_async t stereo_signal ~buffer_size_in_samples =
    assert (bits mod 8 == 0);
    assert (channels == 2);
    let ready_to_swap = Sync_channel.create () in
    let ready_to_gen = Sync_channel.create () in
    let viz_queue = Viz_queue.create () in
    let buffer_size_in_bytes = bits / 8 * channels * buffer_size_in_samples in
    let buffer = Double_buffer.create buffer_size_in_bytes in
    let ctx = { Signal.Ctx.sample_rate_hz = float_of_int rate } in
    let write_bytes_to_buffer =
      match t.byte_format with
      | `LITTLE_ENDIAN | `UNKNOWN | `NATIVE ->
        (* assume little-endianness unless otherwise specified as it's by far the most common *)
        write_bytes_to_buffer_le
      | `BIG_ENDIAN -> write_bytes_to_buffer_be
    in
    let _generator =
      Domain.spawn (fun _ ->
        while true do
          Viz_queue.with_writer viz_queue ~f:(fun writer ->
            for i = 0 to buffer_size_in_samples - 1 do
              let left_sample, right_sample = Stereo_signal.sample stereo_signal ctx in
              Viz_queue.Writer.push writer (left_sample, right_sample);
              let left_lo, left_hi = sample_to_byte_pair_lo_hi left_sample in
              let right_lo, right_hi = sample_to_byte_pair_lo_hi right_sample in
              write_bytes_to_buffer
                buffer.generate_into
                (i * 4)
                ~left_lo
                ~left_hi
                ~right_lo
                ~right_hi
            done);
          Sync_channel.Send.send ready_to_swap.send;
          Sync_channel.Recv.recv ready_to_gen.recv
        done)
    in
    let _player =
      Domain.spawn (fun _ ->
        while true do
          Ao.play t.device (Bytes.to_string buffer.play_from);
          Sync_channel.Recv.recv ready_to_swap.recv;
          Double_buffer.swap buffer;
          Sync_channel.Send.send ready_to_gen.send
        done)
    in
    viz_queue
  ;;
end

module Renderer = struct
  open Tsdl

  type t = { renderer : Sdl.renderer }

  let width = 640
  let height = 480

  let create () =
    (match Sdl.init Sdl.Init.video with
     | Error (`Msg e) -> failwith (Printf.sprintf "Init error: %s" e)
     | Ok () -> ());
    let window, renderer =
      match Sdl.create_window_and_renderer ~w:width ~h:height Sdl.Window.windowed with
      | Error (`Msg e) -> failwith (Printf.sprintf "Create window error: %s" e)
      | Ok x -> x
    in
    Sdl.set_window_title window "AO + SDL demo";
    { renderer }
  ;;

  let rec is_window_closed () =
    let event = Sdl.Event.create () in
    if Sdl.poll_event (Some event)
    then (
      let typ = Sdl.Event.get event Sdl.Event.typ in
      if typ == Sdl.Event.quit
      then (
        Sdl.quit ();
        true)
      else is_window_closed ())
    else false
  ;;

  let render_points t ~f =
    Sdl.set_render_draw_color t.renderer 0 0 0 255 |> Result.get_ok;
    Sdl.render_clear t.renderer |> Result.get_ok;
    Sdl.set_render_draw_color t.renderer 0 255 0 255 |> Result.get_ok;
    let width = float_of_int width in
    let height = float_of_int height in
    f ~add_sample:(fun (x, y) ->
      let x = int_of_float ((x *. width /. 2.) +. (width /. 2.)) in
      let y = int_of_float ((y *. height /. 2.) +. (height /. 2.)) in
      let rect = Sdl.Rect.create ~w:4 ~h:4 ~x ~y in
      Sdl.render_fill_rect t.renderer (Some rect) |> Result.get_ok);
    Sdl.render_present t.renderer
  ;;
end

let const x (_ctx : Signal.Ctx.t) = x

let oscillator_sine ?(offset_01 = 0.0) freq_hz =
  let state_01 = ref offset_01 in
  fun (ctx : Signal.Ctx.t) ->
    let freq_hz = Signal.sample freq_hz ctx in
    let delta = freq_hz /. ctx.sample_rate_hz in
    state_01 := Float.rem (!state_01 +. delta) 1.0;
    sin (!state_01 *. Float.pi *. 2.0)
;;

let make_signal () =
  let freq = 150.0 in
  let left =
    let open Signal.Arith in
    let lfo =
      ((oscillator_sine (const (freq *. 0.49)) + const 1.0) * const 0.25) + const 0.25
    in
    oscillator_sine (const freq + (oscillator_sine (const 0.01) * const 0.5)) * lfo
  in
  let right =
    let open Signal.Arith in
    let lfo =
      ((oscillator_sine ~offset_01:0.25 (const (freq *. 1.51)) + const 1.0) * const 0.25)
      + const 0.25
    in
    oscillator_sine (const (freq *. 1.5)) * lfo
  in
  Stereo_signal.make ~left ~right
;;

let () =
  let stereo_signal = make_signal () in
  let player = Player.create () in
  let viz_queue = Player.run_async player ~buffer_size_in_samples:256 stereo_signal in
  let frame_delay = 1.0 /. 60. in
  let renderer = Renderer.create () in
  let rec loop () =
    if Renderer.is_window_closed ()
    then Player.close player
    else (
      Renderer.render_points renderer ~f:(fun ~add_sample ->
        Viz_queue.drain viz_queue ~f:(fun sample -> add_sample sample));
      Unix.sleepf frame_delay;
      loop ())
  in
  loop ()
;;
