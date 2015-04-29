object ArcBoxForm: TArcBoxForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ArcBoxForm'
  ClientHeight = 201
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = PopupMenu
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ArchDirGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 73
    Caption = #22793#25563#20808#12501#12457#12523#12480'(&F)'
    TabOrder = 0
    object ArchDirComboBox: TAutoComboBox
      Left = 8
      Top = 20
      Width = 257
      Height = 21
      DropDownCount = 32
      TabOrder = 0
      AutoSuggest = asDefault
    end
    object ArchDirButton: TButton
      Left = 268
      Top = 20
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = ArchDirButtonClick
    end
    object UseArchDirCheckBox: TCheckBox
      Left = 8
      Top = 48
      Width = 281
      Height = 17
      Caption = #22793#25563#20803#12398#26360#24235#12392#21516#12376#22580#25152#12395#22793#25563#12377#12427'(&V)'
      TabOrder = 2
      OnClick = UseArchDirCheckBoxClick
    end
  end
  object ArchFormatGroupBox: TGroupBox
    Left = 8
    Top = 88
    Width = 297
    Height = 73
    Caption = #22793#25563#12377#12427#26360#24235#24418#24335'(&E)'
    TabOrder = 1
    object ArchFormatZipRadioButton: TRadioButton
      Left = 8
      Top = 20
      Width = 65
      Height = 17
      Caption = '&ZIP'
      TabOrder = 0
      OnClick = ArchFormatZipRadioButtonClick
    end
    object ArchFormat7zRadioButton: TRadioButton
      Left = 80
      Top = 20
      Width = 65
      Height = 17
      Caption = '&7z'
      TabOrder = 1
      OnClick = ArchFormat7zRadioButtonClick
    end
    object ArchFormatTarRadioButton: TRadioButton
      Left = 152
      Top = 20
      Width = 65
      Height = 17
      Caption = '&TAR'
      TabOrder = 2
      OnClick = ArchFormatTarRadioButtonClick
    end
    object ArchFormatTgzRadioButton: TRadioButton
      Left = 224
      Top = 20
      Width = 65
      Height = 17
      Caption = 'TAR-&GZ'
      TabOrder = 3
      OnClick = ArchFormatTgzRadioButtonClick
    end
    object ArchFormatTbz2RadioButton: TRadioButton
      Left = 8
      Top = 44
      Width = 65
      Height = 17
      Caption = 'TAR-&BZ2'
      TabOrder = 4
      OnClick = ArchFormatTbz2RadioButtonClick
    end
    object ArchFormatTxzRadioButton: TRadioButton
      Left = 80
      Top = 44
      Width = 65
      Height = 17
      Caption = 'TAR-&XZ'
      TabOrder = 5
      OnClick = ArchFormatTxzRadioButtonClick
    end
    object ArchFormatWimRadioButton: TRadioButton
      Left = 152
      Top = 44
      Width = 65
      Height = 17
      Caption = '&WIM'
      TabOrder = 6
      OnClick = ArchFormatWimRadioButtonClick
    end
    object ArchLevelButton: TButton
      Left = 224
      Top = 40
      Width = 65
      Height = 25
      Caption = #35373#23450'(&P)...'
      TabOrder = 7
      OnClick = ArchLevelButtonClick
    end
  end
  object OKButton: TButton
    Left = 136
    Top = 168
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 224
    Top = 168
    Width = 81
    Height = 25
    Caption = #12461#12515#12531#12475#12523
    TabOrder = 3
    OnClick = CancelButtonClick
  end
  object PopupMenu: TPopupMenu
    AutoHotkeys = maManual
    Left = 8
    Top = 8
    object HelpAboutMenuItem: TMenuItem
      Caption = #12496#12540#12472#12519#12531#24773#22577'(&A)...'
      OnClick = HelpAboutMenuItemClick
    end
  end
  object DragDropTarget: TDragDropTarget
    Target = Owner
    OnDragDropFiles = DragDropTargetDragDropFiles
    Left = 40
    Top = 8
  end
end
