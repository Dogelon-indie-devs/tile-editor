unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,
  System.JSON,
  System.Math.Vectors,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Colors,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Viewport3D,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.ListBox,
  FMX.MaterialSources;

type TTileType = (TTT_Emptytile, TTT_Floor, TTT_Wall);
TTileTypeHelper = record helper for TTileType
  function ToString : string;
  class function FromString(input:string) : TTileType; static;
end;

type TTile = class
  x,y: integer;
  tileType: TTileType;
  tileContent: TCustomMesh;
  material: TColorMaterialSource;
  walkable: boolean;
end;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    SpinBox_room_size_x: TSpinBox;
    SpinBox_room_size_y: TSpinBox;
    ColorPicker1: TColorPicker;
    Button1: TButton;
    Viewport3D1: TViewport3D;
    Grid3D1: TGrid3D;
    Dummy1: TDummy;
    Camera1: TCamera;
    Light1: TLight;
    GroupBox2: TGroupBox;
    ComboBox1: TComboBox;
    CheckBox_walkable: TCheckBox;
    Label_last_tile: TLabel;
    ComboBox2: TComboBox;
    Edit_tile_color: TEdit;
    GroupBox3: TGroupBox;
    Switch1: TSwitch;
    Label_camera: TLabel;
    Button2: TButton;
    Button_save: TButton;
    Button_load: TButton;
    SaveDialog1: TSaveDialog;
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ColorPicker1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button_saveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Generate_room;
    procedure Move_camera(X,Y: single);
    procedure Apply_style_to_tile(Sender: TObject);
    function Create_tile_object(tileType: TTileType): TCustomMesh;
    function Create_tile_material(tileType: TTileType): TColorMaterialSource;
    function Create_tile(tile_index: integer; tileType: TTileType): TTile;
  end;

var
  Form1: TForm1;
  fDown: TPointF;
  selecting_tiles: boolean;
  tiles: array of array of TTile;
  tilecount_x, tilecount_y: integer;

implementation

{$R *.fmx}

procedure Destroy_tile(tile_index: integer);
begin
  var x:= tile_index div tilecount_x;
  var y:= tile_index mod tilecount_x;

  var tile:= tiles[x,y];
  tile.tileContent.Free;
  tile.material.Free;
  tile.Free;
  tiles[x,y]:= nil;
end;

function Is_same_tiletype_as_selected(tile_index: integer): boolean;
begin
  var x:= tile_index div tilecount_x;
  var y:= tile_index mod tilecount_x;
  var existing_tile_type:= tiles[x,y].tileType;
  var selected_tile_type:= TTileType.FromString(Form1.ComboBox1.Selected.Text);

  result:= existing_tile_type = selected_tile_type;
end;

procedure Recolor_material(material: TColorMaterialSource; color:TAlphaColor);
begin
  material.Color:= color;
end;

procedure TForm1.Apply_style_to_tile(Sender: TObject);
var x,y: integer;
begin
  with (Sender As TCustomMesh) do
    begin
      x:= tag div tilecount_x;
      y:= tag mod tilecount_x;
    end;

  Label_last_tile.Text:= 'Last tile: X='+x.ToString+', Y='+y.ToString;

  var clicked_tile:= tiles[x,y];
  var tile_index:= clicked_tile.tileContent.Tag;

  if Is_same_tiletype_as_selected(tile_index) then
    begin
      var new_color:= StringToAlphaColor(Edit_tile_color.Text);
      Recolor_material(clicked_tile.material,new_color);
    end
  else
    begin
      Destroy_tile(tile_index);
      var selected_tile_type:= TTileType.FromString(Form1.ComboBox1.Selected.Text);
      tiles[x,y]:= Create_tile(tile_index,selected_tile_type);
    end;
end;

function TForm1.Create_tile_object(tileType: TTileType): TCustomMesh;
begin
  case tileType of
    TTT_Floor:Result:= TPlane.Create(Viewport3D1);
    TTT_Wall: Result:= TCube. Create(Viewport3D1);
    else exit(nil);
  end;

  Result.Width:=  1;
  Result.Height:= 1;
  case tileType of
    TTT_Floor:Result.Depth:= 0.001;
    TTT_Wall: Result.Depth:= 1;
  end;

  Result.Scale.X:=  1;
  Result.Scale.Y:=  1;
  Result.Scale.Z:=  1;

  Result.Opacity:= 1;
  Result.Visible:= true;
  Result.HitTest:= selecting_tiles;
  Result.Projection:= TProjection.Camera;
  Result.Parent:= Viewport3D1;

  Result.OnClick:= Apply_style_to_tile;
end;

function TForm1.Create_tile_material(tileType: TTileType): TColorMaterialSource;
begin
  Result:= TColorMaterialSource.Create(Viewport3D1);
  case tileType of
    TTT_Floor:Result.Color:= TAlphaColorRec.Blue;
    TTT_Wall: Result.Color:= TAlphaColorRec.Gray;
  end;
end;

function TForm1.Create_tile(tile_index: integer; tileType: TTileType): TTile;
begin
  result:= TTile.Create;
  result.x:= tile_index div tilecount_x;
  Result.y:= tile_index mod tilecount_x;
  Result.tileType:= tileType;

  Result.tileContent:= Create_tile_object(tileType);
  Result.tileContent.Name:= 'tile'+result.X.ToString+result.Y.ToString;
  Result.tileContent.Tag:= tile_index;

  Result.tileContent.Position.X:= result.X;
  Result.tileContent.Position.Y:= result.Y;
  Result.tileContent.Position.Z:= 0;
  Result.material:= Create_tile_material(tileType);
  Result.tileContent.MaterialSource:= Result.material;

  Result.walkable:= tileType=TTileType.TTT_Floor;
end;

procedure TForm1.Generate_room;
begin
  tilecount_x:= round(SpinBox_room_size_x.Value);
  tilecount_y:= round(SpinBox_room_size_y.Value);

  SetLength(tiles,tilecount_x,tilecount_y);

  var tile_index:= 0;
  for var x := 0 to tilecount_x-1 do
  for var y := 0 to tilecount_y-1 do
    begin
      var isBorderTile:= (x=0) OR (y=0) OR (x=tilecount_x-1) OR (y=tilecount_y-1);
      var tileType:= TTT_Floor;
      if isBorderTile then
        tileType:= TTT_Wall;

      tiles[x,y]:= Create_tile(tile_index,tileType);
      inc(tile_index);
    end;

  Dummy1.Position.X:= round(tilecount_x/2);
  Dummy1.Position.Y:= round(tilecount_y/2);

  Viewport3D1.Repaint;
end;

procedure Toggle_hittest_for_all_tiles;
begin
  for var x := 0 to tilecount_x-1 do
  for var y := 0 to tilecount_y-1 do
    tiles[x,y].tileContent.HitTest:= selecting_tiles;
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  selecting_tiles:= Switch1.IsChecked;
  Toggle_hittest_for_all_tiles;

  if selecting_tiles then
    Label_camera.Text:= 'Selecting tiles'
  else
    Label_camera.Text:= 'Moving camera';
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled:= false;
  SpinBox_room_size_x.Enabled:= false;
  SpinBox_room_size_y.Enabled:= false;

  Generate_room;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Dummy1.RotationAngle.X := 0;
  Dummy1.RotationAngle.Y := 0;
end;

procedure StringToFile(str,filename:string; append:boolean=false);
begin
  var work:= TStringList.Create;
  try
    if append then
      begin
        if fileExists(filename) then
          work.LoadFromFile(filename);
        work.Add(str);
      end
    else
      work.Text:= str;

    try
      work.SaveToFile(filename,TEncoding.UTF8);
    except
    end;

  finally
    work.Free;
  end;
end;

function Export_layout_as_json: string;
begin
  var JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('RoomName','Lounge');
    var DataArray := TJSONArray.Create;
    for var x := 0 to tilecount_x-1 do
    for var y := 0 to tilecount_y-1 do
      begin
        var tile:= tiles[x,y];
        var DataObject := TJSONObject.Create;
        DataObject.AddPair('X', tile.x.ToString);
        DataObject.AddPair('Y', tile.y.ToString);
        DataObject.AddPair('tileType', tile.tileType.ToString);
        DataObject.AddPair('materialColor', AlphaColorToString(tile.material.Color));
        DataObject.AddPair('walkable', tile.walkable.ToString);
        DataArray.AddElement(DataObject);
      end;
    JSONObject.AddPair('tiles',DataArray);

  finally
    result:= JSONObject.ToString;
    JSONObject.Free;
  end;
end;

procedure TForm1.Button_saveClick(Sender: TObject);
begin
  var json:= Export_layout_as_json;
  SaveDialog1.InitialDir:= GetCurrentDir;
  if SaveDialog1.Execute then
    StringToFile(json,'room_lounge.json');
end;

procedure TForm1.ColorPicker1Click(Sender: TObject);
begin
  var color := ColorPicker1.color;
  Edit_tile_color.Text := AlphaColorToString(color);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  var selected_tile_type:= TTileType.FromString(ComboBox1.Selected.Text);
  CheckBox_walkable.IsChecked:= selected_tile_type=TTileType.TTT_Floor;
end;

procedure TForm1.Move_camera(X,Y: single);
begin
  Dummy1.Position.X:= Dummy1.Position.X + X;
  Dummy1.Position.Y:= Dummy1.Position.Y - Y;
  Camera1.Position.Y:= 0;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if lowercase(KeyChar)='w' then
    Move_camera(0,0.1);
  if lowercase(KeyChar)='a' then
    Move_camera(-0.1,0);
  if lowercase(KeyChar)='s' then
    Move_camera(0,-0.1);
  if lowercase(KeyChar)='d' then
    Move_camera(0.1,0);
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not selecting_tiles then
    begin
      if ssRight in Shift then
        with Dummy1 do
          begin
            RotationAngle.X := RotationAngle.X - (Y - fDown.Y);
            RotationAngle.Y := RotationAngle.Y - (X - fDown.X);
            fDown := PointF(X, Y);
          end;
    end;
end;

procedure TForm1.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if selecting_tiles then exit;

  var new_position := Camera1.Position.Y - (WheelDelta / 100);
  if (new_position > 1) AND (new_position < 200) then
    Camera1.Position.Y := new_position;
end;

{ TTileTypeHelper }

class function TTileTypeHelper.FromString(input:string): TTileType;
begin
  result:= TTileType.TTT_Emptytile;
  if input='Floor' then
    result:= TTileType.TTT_Floor;
  if input='Wall' then
    result:= TTileType.TTT_Wall;
end;

function TTileTypeHelper.ToString: string;
begin
  case self of
    TTT_Emptytile:result:= 'EmptyTile';
    TTT_Floor:    result:= 'Floor';
    TTT_Wall:     result:= 'Wall';
  end;
end;

end.
