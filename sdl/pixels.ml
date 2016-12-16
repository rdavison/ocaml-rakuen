open Base
open Stdio
open Tsdl

let main =
  Random.self_init ();

  let open Result.Monad_infix in
  let open Result.Let_syntax in
  let open Sdl in

  begin
    (* initialize SDL *)
    init Init.everything >>= fun () ->
    Caml.at_exit quit;

    (* create an array to represent our pixels *)
    let texWidth = 1024 in
    let texHeight = 1024 in
    let pixels = Bigarray.(Array1.create Int8_unsigned c_layout (texWidth * texHeight * 4)) in

    (* create window, renderer, and info *)
    let%bind window = create_window "SDL2" 600 600 Window.shown in
    let%bind renderer = create_renderer window ~flags:Renderer.accelerated in
    let%bind info = get_renderer_info renderer in
    let%bind texture = create_texture renderer Pixel.format_argb8888 Texture.access_streaming ~w:texWidth ~h:texHeight in

    (* main loop of the program *)
    let running = ref true in
    while !running do
      let start_time = get_performance_counter () in

      let event = Event.create () in
      begin
        (* clear the screen *)
        set_render_draw_color renderer 0 0 0 1 >>= fun () ->
        render_clear renderer >>= fun () ->

        (* First, handle events *)
        let break = ref false in
        while (poll_event (Some event) && not (!break)) do
          (match Event.(enum (get event typ)) with
          | `Quit -> (running := false; break := true)
          | `Key_down ->
            (match Event.(Scancode.enum (get event keyboard_scancode)) with
            | `Escape -> (running := false; break := true)
            | _ -> ())
          | _ -> ())
        done;

        (* Second, draw a bunch of pixels to the buffer *)
        for i = 0 to 999 do
          let x = Random.int texWidth in
          let y = Random.int texHeight in
          let offset = (texWidth * 4 * y) + x * 4 in
          pixels.{offset + 0} <- Random.int 256;
          pixels.{offset + 1} <- Random.int 256;
          pixels.{offset + 2} <- Random.int 256;
          pixels.{offset + 3} <- 1;
        done;

        (* Update the texture and draw it to the screen *)
        update_texture texture None pixels (texWidth * 4) >>= fun () ->
        render_copy renderer texture >>= fun () ->
        Result.return (render_present renderer) >>= fun () ->

        (* Finally, output some metadata about the computation *)
        let end_time = get_performance_counter () in
        let freq = get_performance_frequency () in
        let seconds = 
          let open Int64 in 
          let diff = end_time - start_time |> to_float in
          let freq = freq |> to_float in
          Caml.(diff /. freq *. 1000.0)
        in
        Result.return (printf "Frame time: %fms\n" seconds)
      end |> function
      | Ok () -> printf "%!" (* %! flushes the output *)
      | Error (`Msg e) -> failwith e
    done;
    destroy_renderer renderer;
    destroy_window window;
    Result.return ()
  end |> function
  | Ok x -> x
  | Error (`Msg e) -> failwith e
