-module(rf_sprite).

-record(sprite_frame, {x, y, w, h}).
-record(sprite, {image, frames}).
-record(sprite_data, {images, sprites}).

-export([load_sprites/0]).

load_images() ->
  ImagesToLoad = application:get_env(red_fields, images, []),
  load_images(ImagesToLoad, dict:new()).

load_images([], Images) ->
  Images;
load_images([{ImageId, ImageLocation} | ImageList], Images) ->
  Image = wxImage:new(full_file_path(ImageLocation)),
  dict:store(ImageId, Image, Images),
  load_images(ImageList, Images).

load_sprites([], Sprites) ->
  Sprites;
load_sprites([{SpriteId, SpriteImage, Frames} | SpritesToLoad], Sprites) ->
  ParsedFrames = parse_frames(Frames, []),
  NewSprites = dict:store(SpriteId, #sprite{image = SpriteImage, frames=ParsedFrames}, Sprites),
  load_sprites(SpritesToLoad, NewSprites).

load_sprites() ->
  Images = load_images(),
  SpritesToLoad = application:get_env(red_fields, sprites, []),
  Sprites = load_sprites(SpritesToLoad, dict:new()),
  #sprite_data{images=Images, sprites=Sprites}.

parse_frames([], ParsedFrames) ->
  lists:reverse(ParsedFrames);
parse_frames([{X, Y, Width, Height} | Frames], ParsedFrames) ->
  NextFrames = [#sprite_frame{x=X, y=Y, w=Width, h=Height} | ParsedFrames],
  parse_frames(Frames, NextFrames).

full_file_path(Path) ->
  filename:join(code:priv_dir(red_fields), Path).
