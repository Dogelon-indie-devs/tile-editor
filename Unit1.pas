unit Unit1;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
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
  System.Math.Vectors,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.ListBox,
  System.UIConsts, FMX.MaterialSources;

type TTileType = (TTT_Emptytile, TTT_Floor, TTT_Wall);

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
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ColorPicker1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
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

procedure TForm1.Apply_style_to_tile(Sender: TObject);
var x,y: integer;
begin
  with (Sender As TCustomMesh) do
    begin
      x:= tag div tilecount_x;
      y:= tag mod tilecount_x;
    end;

  Label_last_tile.Text:= 'Last tile: X='+x.ToString+', Y='+y.ToString;

  var tile:= tiles[x,y];
  tile.material.Color:= StringToAlphaColor(Edit_tile_color.Text);
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
  Result.tileContent.MaterialSource:= Create_tile_material(tileType);

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

procedure TForm1.ColorPicker1Click(Sender: TObject);
begin
  var color := ColorPicker1.color;
  Edit_tile_color.Text := AlphaColorToString(color);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  var selected_tile_type:= ComboBox1.Selected.Text;
  CheckBox_walkable.IsChecked:= selected_tile_type='Floor';
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

end.
