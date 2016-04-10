object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 344
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -88
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 106
  object Memo1: TMemo
    Left = 112
    Top = 96
    Width = 918
    Height = 766
    Margins.Left = 24
    Margins.Top = 24
    Margins.Right = 24
    Margins.Bottom = 24
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 325
    Width = 843
    Height = 19
    Panels = <>
  end
  object Button1: TButton
    Left = 18
    Top = 12
    Width = 79
    Height = 36
    Margins.Left = 24
    Margins.Top = 24
    Margins.Right = 24
    Margins.Bottom = 24
    Caption = 'OCR File...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 18
    Top = 89
    Width = 79
    Height = 36
    Margins.Left = 24
    Margins.Top = 24
    Margins.Right = 24
    Margins.Bottom = 24
    Caption = 'OCR Test'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object ListView1: TListView
    Left = 112
    Top = 8
    Width = 601
    Height = 153
    Columns = <
      item
        AutoSize = True
      end>
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    IconOptions.WrapText = False
    RowSelect = True
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 4
    ViewStyle = vsReport
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 167
    Width = 177
    Height = 152
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 19
    ParentFont = False
    TabOrder = 5
  end
  object TransymOCR1: TTransymOCR
    UpdateSpeed = 100
    OnOCRCompleted = TransymOCR1OCRCompleted
    OnOCRStatusUpdate = TransymOCR1OCRStatusUpdate
    Left = 184
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Bitmap Files|*.bmp|Tiff Files|*.tif'
    Left = 240
    Top = 80
  end
end
