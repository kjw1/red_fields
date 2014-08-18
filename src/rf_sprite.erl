-module(rf_sprite).

-record(sprite, {image, frames}).
-record(sprite_data, {sprites}).

-export([load_sprites/0, get_frame/3]).

load_images() ->
  ImagesToLoad = application:get_env(red_fields, images, []),
  io:format("Loading images: ~p~n", [ImagesToLoad]),
  load_images(ImagesToLoad, dict:new()).

load_images([], Images) ->
  Images;
load_images([{ImageId, ImageLocation} | ImageList], Images) ->
  Image = wxImage:new(full_file_path(ImageLocation)),
  NewImages = dict:store(ImageId, Image, Images),
  load_images(ImageList, NewImages).

load_sprites([], _Images, Sprites) ->
  Sprites;
load_sprites([{SpriteId, SpriteImage, Frames} | SpritesToLoad], Images, Sprites) ->
  io:format("loading sprite ~p, image:~p~n", [SpriteId, SpriteImage]),
  BitmapFrames = create_frame_bitmaps(SpriteImage, Frames, Images),
  NewSprites = dict:store(SpriteId, #sprite{image = SpriteImage, frames=BitmapFrames}, Sprites),
  load_sprites(SpritesToLoad, Images, NewSprites).

create_frame_bitmaps(ImageId, Frames, Images) ->
  lists:map(fun(Frame) -> create_bitmap(ImageId, Frame, Images) end, Frames).

load_sprites() ->
  Images = load_images(),
  SpritesToLoad = application:get_env(red_fields, sprites, []),
  Sprites = load_sprites(SpritesToLoad, Images, dict:new()),
  #sprite_data{sprites=Sprites}.

full_file_path(Path) ->
  filename:join(code:priv_dir(red_fields), Path).

create_bitmap(ImageId, Frame, Images) ->
  Image = dict:fetch(ImageId, Images),
  SubImage = wxImage:getSubImage(Image, Frame),
  Bitmap = wxBitmap:new(SubImage),
  wxImage:destroy(SubImage),
  Bitmap.

get_frame(SpriteId, FrameNum, SpriteData) ->
  Sprite = dict:fetch(SpriteId, SpriteData#sprite_data.sprites),
  lists:nth(FrameNum, Sprite#sprite.frames).

