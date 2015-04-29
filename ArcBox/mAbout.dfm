object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #12496#12540#12472#12519#12531#24773#22577
  ClientHeight = 145
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 8
    Width = 128
    Height = 128
  end
  object Label1: TLabel
    Left = 144
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 144
    Top = 24
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 144
    Top = 40
    Width = 189
    Height = 13
    Caption = 'Copyright '#169' Kuro. All Rights Reserved.'
  end
  object LinkLabel: TLinkLabel
    Left = 144
    Top = 56
    Width = 143
    Height = 17
    Caption = 
      '<a href="http://www.haijin-boys.com/">http://www.haijin-boys.com' +
      '/</a>'
    TabOrder = 0
    OnLinkClick = LinkLabelLinkClick
  end
  object OKButton: TButton
    Left = 256
    Top = 112
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
