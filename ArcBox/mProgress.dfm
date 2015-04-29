object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #26360#24235#12434#22793#25563#12375#12390#12356#12414#12377
  ClientHeight = 82
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ProcessingLabel: TLabel
    Left = 8
    Top = 16
    Width = 377
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 32
    Width = 377
    Height = 9
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 304
    Top = 48
    Width = 81
    Height = 25
    Caption = #12461#12515#12531#12475#12523
    Default = True
    ModalResult = 2
    TabOrder = 1
    OnClick = CancelButtonClick
  end
end
