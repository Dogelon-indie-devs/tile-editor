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
  System.UIConsts;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    SpinBox1: TSpinBox;
    SpinBox2: TSpinBox;
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
    Label1: TLabel;
    ComboBox2: TComboBox;
    Edit_tile_color: TEdit;
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ColorPicker1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  fDown: TPointF;

implementation

{$R *.fmx}

procedure TForm1.ColorPicker1Click(Sender: TObject);
begin
  var color := ColorPicker1.color;
  Edit_tile_color.Text := AlphaColorToString(color);
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssRight in Shift then
    with Dummy1 do
      begin
        RotationAngle.X := RotationAngle.X - (Y - fDown.Y);
        RotationAngle.Y := RotationAngle.Y - (X - fDown.X);
        fDown := PointF(X, Y);
      end;
end;

procedure TForm1.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  var new_position := Camera1.Position.Z - (WheelDelta / 100);
  if (new_position > 1) AND (new_position < 200) then
    Camera1.Position.Z := new_position;
end;

end.
