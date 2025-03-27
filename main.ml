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

module Player = struct
  let bits = 16
  let rate = 44100
  let channels = 2

  type t = { device : Ao.t }

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
    let device = Ao.open_live ~bits ~rate ~channels ~driver () in
    { device }
  ;;

  let run_async t =
    let ready_to_swap = Sync_channel.create () in
    let ready_to_gen = Sync_channel.create () in
    let buffer = Double_buffer.create 1024 in
    let _generator =
      Domain.spawn (fun _ ->
        let seq = ref 0 in
        while true do
          Bytes.iteri
            (fun i _ ->
               if i / 2 mod 2 == 0
               then Bytes.set buffer.generate_into i (char_of_int (!seq mod 256))
               else
                 Bytes.set buffer.generate_into i (char_of_int (!seq * 100 / 99 mod 256));
               if i mod 16 == 0 then seq := !seq + 1)
            buffer.generate_into;
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

let () =
  let player = Player.create () in
  Player.run_async player;
  let rec loop () =
    print_endline "hello";
    Unix.sleepf 1.0;
    loop ()
  in
  loop ()
;;
