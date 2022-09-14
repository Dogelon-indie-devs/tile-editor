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
    CheckBox1: TCheckBox;
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
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Generate_room;
    procedure Move_camera(X,Y: single);
    procedure Apply_style_to_tile(Sender: TObject);
  end;

type TTileType = (TTT_Emptytile, TTT_Floor, TTT_Wall);

type TTile = class
  x,y: integer;
  tileType: TTileType;
  tileContent: TCustomMesh;
  material: TColorMaterialSource;
  walkable: boolean;
end;


const grid_offset = 0.5;
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

procedure TForm1.Generate_room;
var tile_index: integer;

  function Create_tile(x,y: integer; tileType: TTileType): TTile;
  begin
    result:= TTile.Create;
    result.x:= x;
    Result.y:= y;
    Result.tileType:= tileType;

    case tileType of
      TTT_Floor:Result.tileContent:= TPlane.Create(Viewport3D1);
      TTT_Wall: Result.tileContent:= TCube. Create(Viewport3D1);
      else exit;
    end;

    Result.tileContent.Position.X:= X;
    Result.tileContent.Position.Y:= Y;
    Result.tileContent.Position.Z:= 0;

    Result.tileContent.Width:=  1;
    Result.tileContent.Height:= 1;
    case tileType of
      TTT_Floor:Result.tileContent.Depth:= 0.001;
      TTT_Wall: Result.tileContent.Depth:= 1;
    end;

    Result.tileContent.Scale.X:=  1;
    Result.tileContent.Scale.Y:=  1;
    Result.tileContent.Scale.Z:=  1;

    Result.material:= TColorMaterialSource.Create(Result.tileContent);
    case tileType of
      TTT_Floor:Result.material.Color:= TAlphaColorRec.Blue;
      TTT_Wall: Result.material.Color:= TAlphaColorRec.Gray;
    end;

    Result.tileContent.MaterialSource:= Result.material;
    Result.tileContent.Opacity:= 1;
    Result.tileContent.Visible:= true;
    Result.tileContent.HitTest:= selecting_tiles;
    Result.tileContent.Projection:= TProjection.Camera;
    Result.tileContent.Parent:= Viewport3D1;
    Result.tileContent.Name:= 'tile'+X.ToString+Y.ToString;
    Result.tileContent.Tag:= tile_index;

    Result.tileContent.OnClick:= Apply_style_to_tile;

    Result.walkable:= tileType=TTileType.TTT_Floor;
  end;

begin
  tilecount_x:= round(SpinBox_room_size_x.Value);
  tilecount_y:= round(SpinBox_room_size_y.Value);

  SetLength(tiles,tilecount_x,tilecount_y);

  tile_index:= 0;
  for var x := 0 to tilecount_x-1 do
  for var y := 0 to tilecount_y-1 do
    begin
      var isBorderTile:= (x=0) OR (y=0) OR (x=tilecount_x-1) OR (y=tilecount_y-1);
      var tileType:= TTT_Floor;
      if isBorderTile then
        tileType:= TTT_Wall;

      tiles[x,y]:= Create_tile(x,y,tileType);
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
