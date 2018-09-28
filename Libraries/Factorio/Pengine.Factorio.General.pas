unit Pengine.Factorio.General;

interface

type

  TCategory = class
  private
    FName: string;

  public
    property Name: string read FName;

  end;

  TCategorized = class
  private
    FCategory: TCategory;

  public
    property Category: TCategory read FCategory;

  end;

  TItem = class(TCategorized)
  private
    FName: string;
    FIcon: string;
    FGroup: string;
    FStackSize: Integer;

  public
    property Name: string read FName;
    property Icon: string read FIcon;
    property Group: string read FGroup;
    property StackSize: Integer read FStackSize;

  end;

  TRecipe = class(TCategorized)
  private
    FName: string;


  end;

implementation


end.
