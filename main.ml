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

module Signal = struct
  module Ctx = struct
    type t = { sample_rate_hz : float }
  end

  type t = Ctx.t -> float

  let sample (t : t) ctx = t ctx
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
          for i = 0 to buffer_size_in_samples - 1 do
            let left_sample, right_sample = Stereo_signal.sample stereo_signal ctx in
            let left_lo, left_hi = sample_to_byte_pair_lo_hi left_sample in
            let right_lo, right_hi = sample_to_byte_pair_lo_hi right_sample in
            write_bytes_to_buffer
              buffer.generate_into
              (i * 4)
              ~left_lo
              ~left_hi
              ~right_lo
              ~right_hi
          done;
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
    ()
  ;;
end

let const x (_ctx : Signal.Ctx.t) = x

let oscillator_sine freq_hz =
  let state_01 = ref 0.0 in
  fun (ctx : Signal.Ctx.t) ->
    let freq_hz = Signal.sample freq_hz ctx in
    let delta = freq_hz /. ctx.sample_rate_hz in
    state_01 := Float.rem (!state_01 +. delta) 1.0;
    sin (!state_01 *. Float.pi *. 2.0)
;;

let () =
  let player = Player.create () in
  let stereo_signal =
    Stereo_signal.make
      ~left:(oscillator_sine (const 200.))
      ~right:(oscillator_sine (const 300.))
  in
  Player.run_async player ~buffer_size_in_samples:256 stereo_signal;
  let rec loop () =
    print_endline "hello";
    Unix.sleepf 1.0;
    loop ()
  in
  loop ()
;;
