unit pdfObjs;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type

    pdfDataType = (
        pdfNull=0,
        pdfBool,
        pdfInt,
        pdfFloat,
        pdfRef,
        pdfStr,
        pdfHexStr,
        pdfName,
        pdfArray,
        pdfDict
    );




  PPdfObj = ^TPdfObj;
  TPdfObj = packed record
            private
              FObjType: pdfDataType;
                  FRef: integer;
              procedure Finalize;
            public
              class function NewNull: PPdfObj;static;
              class function NewBool(AValue:Boolean) : PPdfObj;static;
              class function NewInt(AValue: Integer): PPdfObj;static;
              class function NewFloat(AValue: Double): PPdfObj;static;
              class function NewRef(ANum,AGen: Cardinal): PPdfObj;static;
              class function NewName(const S: String): PPdfObj;static;
              class function NewName(S: PChar; ALen: Cardinal): PPdfObj;static;
              class function NewStr(const S: String): PPdfObj;static;
              class function NewStr(S: PChar; ALen: Cardinal): PPdfObj;static;
              class function NewHexStr(const S: String): PPdfObj;static;
              class function NewHexStr(S: PChar; ALen: Cardinal): PPdfObj;static;
              class function NewArray(ACapacity: Cardinal=0): PPdfObj;static;
              class function NewDictionary(ACapacity: Cardinal=0): PPdfObj;static;

              class procedure Release(var Obj: PPdfObj);static;
              procedure Release;
              function Use: PPdfObj;

              property ObjType: pdfDataType read FObjType;

              function AddItem( AnItem: PPdfObj; Key: PPdfObj):boolean;
              function AddItem( AnItem: PPdfObj):boolean;

              function CompareTo( I : Integer ) : Integer; overload;
              function CompareTo(const S: String  ): Integer;overload;
              function CompareTo(S: PChar; ALen: Integer): Integer;overload;
          end;





implementation


type

    // Records for type casting

    pdfNullObj = TPdfObj;

    pdfBoolObj = packed record
                hdr : TPdfObj;
                b : boolean;
    end;

    PPdfIntObj= ^pdfIntObj;
    pdfIntObj = packed record
                hdr : TPdfObj;
                i : Integer;
                class function SafeCast( Obj: TPdfObj): PPdfIntObj;static;
    end;

    pdfFloatObj = packed record
                 hdr : TPdfObj;
                 f : Double;
    end;

    pdfRefObj = packed record
                   hdr : TPdfObj;
               num,gen : Cardinal;
    end;

    PPdfStrObj = ^pdfStrObj;
    pdfStrObj = packed record
                 hdr : TPdfObj;
                 len : Cardinal;
               chars : Array[00..00] of char;
               class function SafeCast( Obj: TPdfObj): PPdfStrObj;static;
    end;


    PPdfArrayObj=^pdfArrayObj;
    pdfArrayObj = packed record
                  hdr: TPdfObj;
                  len,
             capacity: cardinal;
                items: ^PPdfObj;
                 class function SafeCast( Obj: TPdfObj): PPdfArrayObj;static;
    end;


    pdfDictItem = packed record
              key,value: PPdfObj;
    end;

    PPdfDictObj= ^pdfDictObj;
    pdfDictObj = packed record
                  hdr: TPdfObj;
                  len,
             capacity: cardinal;
                items: ^pdfDictItem;
               class function SafeCast( Obj: TPdfObj): PPdfDictObj;static;
    end;



    {Global Hash table to store pdf Name Objects}


  // Record to keep collisions in hash table
  PSlotRec = ^TSlotRec;
  TSlotRec = record
              FCount,
              FCapacity: PtrUInt;
              FNames: ^PPdfObj;
              Procedure Init;
              procedure Done;
              procedure Add(Obj: PPdfObj);
   end;

   TPdfNames= class
             FSlots: PSlotRec;
             FSize,
             FCount,
             FCapacity: PtrUInt;
             constructor Create(AHashSize: PtrUInt=0);
             Destructor Destroy;
             function Add(S: PChar; ALen: Integer): PPdfObj;
   end;


function Min(a, b: Integer): Integer;inline;
begin
      if a < b then Result := a
               else Result := b;
end;

function Max(a, b: Integer): Integer;inline;
begin
      if a > b then Result := a
               else Result := b;
end;

{inline SMP check and normal lock.
the locked one is so slow, inlining doesn't matter}
function declocked(var l : longint) : boolean; inline;

begin
  if ismultithread then  l:=InterLockedDecrement(l)
                   else  dec(l);
 declocked:=l=0;
end;

procedure inclocked(var l : longint); inline;

begin
  if ismultithread then InterLockedIncrement(l)
                   else inc(l);
end;


{ PDF Objects }

var

    // Singletons
    TheNullObj: pdfNullObj = (FObjType:pdfNull; FRef:-1);
    TheTrueObj: pdfBoolObj = (hdr:(FObjType:pdfBool; FRef:-1); b: true);
    TheFalseObj: pdfBoolObj = (hdr:( FObjType:pdfBool;FRef:-1); b: false);


    TheNamesManager: TPdfNames = nil;

const

   DefaultDelta = 8;

class function pdfArrayObj.SafeCast( Obj: TPdfObj): PPdfArrayObj;inline;
begin
  if (@Obj=nil) or (obj.ObjType<>pdfArray) then result:=nil
                                           else result:=PPdfArrayObj(pointer(@Obj));

end;

class function pdfDictObj.SafeCast( Obj: TPdfObj): PPdfDictObj;inline;
begin
  if (@Obj=nil) or (obj.ObjType<>pdfDict) then result:=nil
                                          else result:=PPdfDictObj(pointer(@Obj));

end;

class function pdfStrObj.SafeCast( Obj: TPdfObj): PPdfStrObj;inline;
begin
  if (@Obj=nil) or
     not (obj.ObjType in [pdfStr,pdfHexStr,pdfName])
     then result:=nil
     else result:=PPdfStrObj(pointer(@Obj));

end;

class function pdfIntObj.SafeCast( Obj: TPdfObj): PPdfIntObj;inline;
begin
 if (@Obj=nil) or (Obj.ObjType<>pdfInt) then Result:=nil
    else result:=PPdfIntObj(pointer(@Obj));
end;

class function TPdfObj.NewNull: PPdfObj;inline;
begin
 Result:= @TheNullObj;
end;

class function TPdfObj.NewBool(AValue: Boolean):PPdfObj;inline;
begin
   if AValue then Result:=@TheTrueObj
             else Result:=@TheFalseObj;
end;


class function TPdfObj.NewInt(AValue: Integer):PPdfObj;
begin
   Result := GetMem(SizeOf(pdfIntObj));
   with pdfIntObj(Pointer(Result)^) do
    begin
      hdr.FRef:=1;
      hdr.FObjType:=pdfInt;
      i:=AValue;
    end;
end;


class function TPdfObj.NewFloat(AValue: Double): PPdfObj;
begin
   Result := GetMem(SizeOf(pdfFloatObj));
   with pdfFloatObj(Pointer(Result)^) do
    begin
     hdr.FRef:=1;
     hdr.FObjType:=pdfFloat;
     f:=AValue;
    end;
end;

class function TPdfObj.NewRef(ANum,AGen: Cardinal): PPdfObj;static;
begin
   Result := GetMem(SizeOf(pdfRefObj));
   with pdfRefObj(Pointer(Result)^) do
    begin
     hdr.FRef:=1;
     hdr.FObjType:=pdfRef;
     num:=ANum;
     gen:=AGen;
    end;
end;

function NewStringObject(ObjType: pdfDataType; S: PChar; ALen: Cardinal): PPdfObj;
begin
  Result := GetMem(SizeOf(pdfStrObj)+ALen);
   with pdfStrObj(Pointer(Result)^) do
    begin
     hdr.FRef:=1;
     hdr.FObjType:=ObjType;
     len:=ALen;
     Move(S^,chars[0],len);
     Chars[len]:=#0;
    end;
end;

class function TPdfObj.NewName(S: PChar; ALen: Cardinal): PPdfObj;inline;
begin
 if TheNamesManager=nil then Result:=NewStringObject(pdfName,S,ALen)
                        else Result:=TheNamesManager.Add(S,ALen)^.Use;
end;

class function TPdfObj.NewName(const S: String): PPdfObj;inline;
begin
  Result:=NewName(@S[1],system.Length(S));
end;

class function TPdfObj.NewStr(S: PChar; ALen: Cardinal): PPdfObj;inline;
begin
  Result:=NewStringObject(pdfStr,S,ALen);
end;

class function TPdfObj.NewStr(const S: String): PPdfObj;inline;
begin
  Result:=NewStringObject(pdfStr,@S[1],system.Length(S));
end;

class function TPdfObj.NewHexStr(S: PChar; ALen: Cardinal): PPdfObj;inline;
begin
  Result:=NewStringObject(pdfHexStr,S,ALen);
end;

class function TPdfObj.NewHexStr(const S: String): PPdfObj;inline;
begin
  Result:=NewStringObject(pdfHexStr,@S[1],system.Length(S));
end;

class function TPdfObj.NewArray(ACapacity: Cardinal=0): PPdfObj;
begin
   Result := GetMem(SizeOf(pdfArrayObj));
   with pdfArrayObj(Pointer(Result)^) do
    begin
     hdr.FRef := 1;
     hdr.FObjType := pdfArray;
     len:=0;
     if ACapacity=0 then ACapacity:=DefaultDelta;
     capacity := ACapacity;
     if capacity<>0 then items := Getmem(capacity*sizeOf(PPdfObj))
                    else items := nil;
    end;
end;

class function TPdfObj.NewDictionary(ACapacity: Cardinal=0): PPdfObj;
begin
   Result := GetMem(SizeOf(pdfDictObj));
   with pdfDictObj(Pointer(Result)^) do
    begin
     hdr.FRef := 1;
     hdr.FObjType := pdfDict;
     len:=0;
     if ACapacity=0 then ACapacity:=DefaultDelta;
     capacity := ACapacity;
     if capacity<>0 then items := Getmem(capacity*sizeOf(pdfDictItem))
                    else items := nil;
    end;
end;


procedure TPdfObj.Finalize;
var i: integer;
begin
  Case objType of
    pdfArray: with pdfArrayObj(Pointer(@self)^) do
               if Assigned(items) then
               begin
                for i:=00 to len-1 do items[i]^.Release;
                Freemem(items);
               end;
     pdfDict: with pdfDictObj(Pointer(@self)^) do
               if assigned(items) then
              begin
                for i:=00 to len-1 do
                 begin
                  items[i].key^.Release;
                  items[i].value^.Release;
                 end;
                Freemem(items);
              end;
    end;
  Freemem(@Self);
end;

procedure TPdfObj.Release;
begin
  if not Assigned(@Self) then exit;
  if FRef<0 then exit;
  if declocked(FRef) then Finalize;
end;

class procedure TPdfObj.Release(var Obj: PPdfObj);
begin
  Obj^.Release;
  Obj:=nil;
end;

function TPdfObj.Use: PPdfObj ;inline;
begin
  if FRef>0 then inclocked(FRef);
  Result:=@Self;
end;

function TPdfObj.AddItem( AnItem: PPdfObj):boolean;
var A: PPdfArrayObj;
begin
  A:= pdfArrayObj.SafeCast(Self);
  if A<>nil then with A^ do
   begin
    inc(len);
    if len>=capacity then
     begin
       inc(capacity,DefaultDelta);
       items:=ReAllocMem(items,capacity*sizeOf(PPdfObj));
     end;
    items[len]:=AnItem;
    Result:=true;
   end
   else result := false
end;

function TPdfObj.AddItem( AnItem: PPdfObj; Key: PPdfObj):boolean;
var D: PPdfDictObj;
begin
  D:=pdfDictObj.SafeCast(Self);
  if (D=nil) or
     (AnItem=nil) or
     (Key=nil) or
     (Key^.ObjType<>pdfName) then Result := false
  else with D^ do
   begin
    inc(len);
    if len>=capacity then
     begin
       inc(capacity,DefaultDelta);
       items:=ReAllocMem(items,capacity*sizeOf(pdfDictItem));
     end;
    items[len].value:=AnItem;
    items[len].key:=Key;
    Result:=true
   end
end;

function TPdfObj.CompareTo( I : Integer): Integer;
var IObj: PPdfIntObj;
begin
  IObj:=pdfIntObj.SafeCast(Self);
  if IObj = nil then exit (2); // not a string object
  if IObj^.i=i then exit (0);
  if IObj^.i<i then exit(-1)
   else Result:=1;
end;

function TPdfObj.CompareTo(S: PChar; ALen: Integer): Integer;
var Str: PPdfStrObj;
begin
  Str:=pdfStrObj.SafeCast(Self);
  if Str = nil then exit (2); // not a string object
  Result:=CompareChar(S[0],Str^.chars[0],min(ALen,Str^.len))
end;


function TPdfObj.CompareTo(const S: String): Integer;inline;
begin
 CompareTo(@S[1],system.Length(S));
end;




// TPdfNames

const GrowDelta=16;


DefHashSizeIndex=4;
PrimesArray:array[00..25]  of DWord=(53,97,193,389,769,1543,3079,6151,12289,
                                     24593,49157,98317,196613,393241,786433,
                                     1572869,3145739,6291469,12582917,25165843,
                                     50331653,100663319,201326611,402653189,
                                     805306457,1610612741);


// Hash Slots
procedure TSlotRec.Init;
begin
  FCount:=0;
  FCapacity:=0;
  FNames:=nil;
end;

procedure TSlotRec.Done;
var i : integer;
begin
  if FNames<> nil  then
   begin
     for i:=0 to FCount-1 do TPdfObj.Release(FNames[i]);
     FreeMem(FNames);
     FNames:=nil;
   end;
  FCount:=0;
  FCapacity:=0;
end;

procedure TSlotRec.Add(Obj: PPdfObj);
begin
  if FCapacity<=FCount then
   begin
     Inc(FCapacity,GrowDelta);
     FNames:=ReAllocMem(FNames,FCapacity);
   end;
  FNames[FCount]:=Obj;
  Inc(FCount);
end;

constructor TPdfNames.Create(AHashSize: PtrUInt=0);
var i : Integer;
begin
  Inherited Create;
  if AHashSize=0 then AHashSize:=DefHashSizeIndex;
  FSize:=PrimesArray[AHashSize];
  FSlots:=GetMem(FSize*SizeOf(PSlotRec));
  for i:=0 to FSize-1  do FSlots[i].Init;
  FCapacity:=0;
  FCount:=0;
end;

destructor TPdfNames.Destroy;
var i : Integer;
begin
 for i:=0 to FSize-1  do FSlots[i].Done;

 inherited Destroy;
end;

function TPdfNames.Add(S: PChar; ALen: Integer): PPdfObj;
var    H: DWord;
    Slot: PtrUInt;
       i: Integer;
       N: PPdfObj;

function Hash:DWord; // FNV1a hash function
var j: integer;
const
    FNV_offset_basis = 2166136261;
    FNV_prime = 16777619;
begin
    Result := FNV_offset_basis;
    for j := 0 to ALen-1 do
        Result := (Result xor Byte(s[j])) * FNV_prime;
end;

begin
 H:=Hash;
 Slot:=H mod FSize;
 Result:=nil;
 for i:=0 to FSlots[Slot].FCount-1 do
   begin
     N:=FSlots[Slot].FNames[i];
     if N^.CompareTo(S)=0 then exit(N); //found in hash
   end;

  Result:=NewStringObject(pdfName,S,ALen);

  FSlots[Slot].Add(Result);
  Inc(FCount);;
end;

initialization
 TheNamesManager:= TPdfNames.Create;
finalization
 TheNamesManager.Free;
 THeNamesManager:=nil;
end.

