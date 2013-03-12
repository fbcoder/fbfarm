'-----------
' Constants
'-----------
Const NULL = 0
Const MAP_MAX_WIDTH as integer = 40
Const MAP_MAX_HEIGHT as integer = 10

Const TICKS_PER_DAY = 300

'--------------
' Enumerations
'--------------
Enum Boolean
    False = 0
    True = not(false)
End Enum

Enum TileType
  Fence = 0
  Water = 1
  Soil = 2
  Building = 3
End Enum

Enum CropPhase
  None = 0
  Seed = 1
  Harvest = 2
  Rotten = 3
End Enum

'--------------------------------
' Globals
'--------------------------------

'--------------------------------
' Helper Functions Declarations
'--------------------------------

'---------------
' Types
'---------------

Type Logger
	Private:
		lineLength As Integer = (640 \ 8)
		row As Integer = 23
		logfile As String = ""
		toFile As Boolean = Boolean.False
	Public:
		Declare Constructor ()
		Declare Sub debug ( msg As String ) 					
End Type

Constructor Logger ()
End Constructor

Sub Logger.debug ( msg As String )
	Color 7,0
	Locate row,1
	Print String(lineLength," ")
	Dim As String _output = "debug: " & msg	
	Dim length As Integer = Len(_output)
	If length > lineLength Then
		_output = Mid(_output,1,lineLength - 4)
		_output = _output & " ..."
		length = lineLength
	End If
	Locate row,1
	'Color 7,0
	Print Mid(_output,1,length)
	'print _output
End Sub

Type Tile
  private:
    background as integer = 6
    foreground as integer = 6
    char as integer = 220
  public:
    Declare Constructor()
    Declare Constructor(b as integer, f as integer, c as integer)
    Declare Sub drawMe(y as integer, x as integer)
    Declare Sub invert()
End Type

Constructor Tile()
End Constructor

Constructor Tile(b as integer, f as integer, c as integer)
  background = b
  foreground = f
  char = c
End Constructor

Sub Tile.drawMe(y as integer, x as integer)
  locate y,x
  color foreground, background
  print chr$(char)
End Sub

Sub Tile.invert()
  swap foreground,background
End Sub

Type Crop
  private:
    _name As String
    seedPrice as integer
    harvestPrice as integer
    growTime as integer
    harvestTime as integer
    needsWater as Boolean = False
    seedTile as Tile
    growTile as Tile
    rotTile as Tile
  public:
    Declare Constructor(__name As String,_seedPrice as Integer,_harvestPrice as integer,_growTime as integer,_harvestTime as integer,_seedTile as Tile,_growTile as Tile,_rotTile as Tile)
    Declare Function getName() As String
    Declare Function getGrowTime() as integer
    Declare Function getHarvestTime() as integer
    Declare Function getSeedPrice as integer
    Declare Function getHarvestPrice() as integer
    Declare Function getSeedTile() as Tile
    Declare Function getGrowTile() as Tile
    Declare Function getRotTile() as Tile
End Type

Constructor Crop(__name As String,_seedPrice as integer,_harvestPrice as integer,_growTime as integer,_harvestTime as integer,_seedTile as Tile,_growTile as Tile,_rotTile as Tile)
  _name = __name
  seedPrice = _seedPrice
  harvestPrice = _harvestPrice
  growTime = _growTime
  harvestTime = _harvestTime
  seedTile = _seedTile
  growTile = _growTile
  rotTile = _rotTile  
End Constructor

Function Crop.getName() As String
	Return _name
End Function

Function Crop.getGrowTime() as integer
  return growTime
End Function

Function Crop.getHarvestTime() as integer
  return harvestTime
End Function

Function Crop.getSeedPrice() As Integer
	Return seedPrice
End Function

Function Crop.getHarvestPrice() As Integer
	Return harvestPrice
End Function

Function Crop.getSeedTile() as Tile
  return seedTile
End Function

Function Crop.getGrowTile() as Tile
  return growTile
End Function

Function Crop.getRotTile() as Tile
  return rotTile
End Function

Type SoilData
  private:
    plowed as Boolean = Boolean.False
    seed as Crop ptr = NULL
    seedDays as integer = 0
    watered as Boolean = Boolean.False
    _cropPhase as CropPhase = CropPhase.None
  public:
    Declare Constructor()
    Declare Sub plow()
    Declare Function plant(_seed as Crop ptr, byref _tile as Tile) As Boolean
    Declare Sub newDay(byref _tile as Tile)
    Declare Sub killCrop()
    Declare Function getStateString() As String
    Declare Function harvest() As Crop ptr
End Type

Constructor SoilData()
End Constructor

Sub SoilData.plow()
  if plowed <> Boolean.True then
    plowed = Boolean.True
  else
    ' already plowed
  end if
End Sub

Function SoilData.plant(_seed as Crop ptr, byref _tile as Tile) As Boolean
  if plowed then
    if _seed <> NULL then
      ' seed can be planted.
      seed = _seed
      _tile = seed->getSeedTile
      _cropPhase = CropPhase.Seed
      Return Boolean.True
    end if
  else
    locate 23,3
    print "debug: Tile must be plowed before seeding."
  end If
  Return Boolean.False
End Function

Sub SoilData.newDay(byref _tile as Tile)
  if seed <> NULL then
    seedDays += 1  
    dim gt as integer = seed->getGrowTime()
    dim ht as integer = seed->getHarvestTime()
    if seedDays = gt then
      ' Crop is ready to harvest
      _tile = seed->getGrowTile()
      _cropPhase = CropPhase.Harvest
    elseif seedDays > gt then
      if (seedDays - gt) > ht then
	' Waited too long, crop gone.
	_tile = seed->getRotTile()
	_cropPhase = CropPhase.Rotten
      end if
    end if
  end if
End Sub

Sub SoilData.killCrop()
  seed = NULL
  seedDays = 0
End Sub

Function SoilData.getStateString() As String
	Dim As String phaseString, cropString = ""	
	If seed <> NULL Then
		cropString = seed->getName()
	Else		
		cropString = "Plowed patch of land."
	EndIf
	phaseString = ""
	Select Case _cropPhase:
		Case CropPhase.Seed:
			phaseString = "Seed of "
		Case CropPhase.Harvest:
			phaseString = "Fullgrown "
		Case CropPhase.Rotten:
			phaseString = "Rotten bush of "
	End Select
	Return phaseString & cropString & "."
End Function

Function SoilData.harvest() As Crop Ptr
	If seed <> NULL Then
		If _cropPhase = CropPhase.Harvest Then
			Return seed
		EndIf
	EndIf
	Return NULL
End Function

'=======================
' Tile globals 'yuck' :P
'=======================
' Fence tiles
Dim Shared FENCE_HORIZONTAL as Tile = Tile(6,7,205)
Dim Shared FENCE_VERTICAL as Tile = Tile(6,7,186)
Dim Shared FENCE_TOP_LEFT as Tile = Tile(6,7,201)
Dim Shared FENCE_TOP_RIGHT as Tile = Tile(6,7,187)
Dim Shared FENCE_BOTTOM_LEFT as Tile = Tile(6,7,200)
Dim Shared FENCE_BOTTOM_RIGHT as Tile = Tile(6,7,188)
' Soil tiles 'fallow'
Dim shared SOIL_LIGHT as Tile = Tile(0,6,176) 
Dim shared SOIL_MEDIUM as Tile = Tile(0,6,177) 
Dim shared SOIL_FULL as Tile = Tile(0,6,178)
' Soil modified
Dim shared SOIL_PLOWED as Tile = Tile(6,6,219)
'=======================
'  Crop Globals
'=======================
' Clover Plants
Dim shared SOIL_SEEDED as Tile = Tile(6,14,249)
Dim shared CROP_CLOVER_GROWN as Tile = Tile(6,2,5)
Dim shared CROP_CLOVER_ROT as Tile = Tile(6,8,5)
Dim Shared CROP_CLOVER as Crop = Crop("Lucky Clover",10,15,10,10,SOIL_SEEDED,CROP_CLOVER_GROWN,CROP_CLOVER_ROT)

'================
' The visual map
'================
Type Map
  private:
    logPtr As Logger Ptr = NULL
    height as Integer = 0
    width as Integer = 0
    visualTile(MAP_MAX_HEIGHT,MAP_MAX_WIDTH) as Tile
    _soilData(MAP_MAX_HEIGHT,MAP_MAX_WIDTH) as SoilData ptr
    typeMap(MAP_MAX_HEIGHT,MAP_MAX_WIDTH) as TileType
  public:
    Declare Constructor()
    Declare Destructor()
    Declare Constructor(w as integer, h as Integer, _logPtr As Logger ptr)
    Declare Sub changeTile(row as integer, col as integer, newTile as Tile)
    Declare Sub drawMap(rowOffset as integer, colOffset as integer)
    Declare Sub plow(row as integer, col as integer)
    Declare Function seed(row as integer, col as integer, cropPtr as Crop ptr) As Boolean
    Declare Function harvest(row as integer, col as integer) As Crop ptr
    Declare Function walkAble(row as integer, col as integer) as Boolean
    Declare Sub update()
    Declare Function getTileInfo(row As Integer, col As Integer) as String
End Type

Constructor Map ()
End Constructor

Constructor Map (w as integer, h as Integer, _logPtr As Logger ptr)
  logPtr = _logPtr
  height = h
  width = w
  Dim row_top as integer = 0
  Dim row_bottom as integer = height - 1
  Dim col_left as integer = 0
  Dim col_right as integer = width - 1
  
  'vertical fence
  For i as integer = 1 to (height - 2)
    visualTile(i,col_left) = FENCE_VERTICAL
    visualTile(i,col_right) = FENCE_VERTICAL
  Next i
  
  'horizontal fence
  For i as integer = 1 to (width - 2)
    visualTile(row_top,i) = FENCE_HORIZONTAL
    visualTile(row_bottom,i) = FENCE_HORIZONTAL
  Next i
  
  'topleft
  visualTile(row_top,col_left) = FENCE_TOP_LEFT
  
  'topright
  visualTile(row_top,col_right) = FENCE_TOP_RIGHT
  
  'bottomleft
  visualTile(row_bottom,col_left) = FENCE_BOTTOM_LEFT
  
  'bottomright
  visualTile(row_bottom,col_right) = FENCE_BOTTOM_RIGHT
  
  'randomly fill rest
  For row as integer = 1 to (height -2)
    For col as integer = 1 to (width - 2)
      Dim r as integer = int(rnd() * 3)
      Select Case r
	Case 0:
	  visualTile(row,col) = SOIL_LIGHT
	Case 1:
	  visualTile(row,col) = SOIL_MEDIUM
	Case 2:
	  visualTile(row,col) = SOIL_FULL
      End Select
    Next col
  Next row
  
	For row as integer = 0 to (height - 1)
		For col as integer = 0 to (width - 1)
			If row > 0 and row < (height - 1) and col > 0 and col < (width - 1) then
				typeMap(row,col) = TileType.Soil
			Else
				typeMap(row,col) = TileType.Fence
			End if 
		Next col
	Next row
End Constructor

Destructor Map()
  For i as integer = 0 to (height - 1)
    For j as integer = 0 to (width - 1)
      If _soilData(i,j) <> NULL Then
      	Delete _soilData(i,j)
      EndIf
    Next j
  Next i  
End Destructor

Sub Map.drawMap(rowOffset as integer, colOffset as integer)
  For i as integer = 0 to (height - 1)
    For j as integer = 0 to (width - 1)
      visualTile(i,j).drawMe(i + (1 + rowOffset),j + (1 + colOffset))
    Next j
  Next i  
End Sub

Sub Map.changeTile(row as integer, col as integer, newTile as Tile)
  visualTile(row,col) = newTile
End Sub

Sub Map.plow(row as integer, col as integer)
	If typeMap(row,col) = TileType.Soil Then        
    	If _soilData(row,col) = NULL Then:
    		_soilData(row,col) = New SoilData()
    	EndIf
    	changeTile(row,col,SOIL_PLOWED)
    	_soilData(row,col)->plow()
  	Else
		logPtr->debug("Unplowable tile.")
	EndIf
End Sub

Function Map.seed ( row as Integer, col as Integer, cropPtr as Crop Ptr ) As Boolean
  if _soilData(row,col) <> NULL And typeMap(row,col) = TileType.Soil then            
    If _soilData(row,col)->plant(cropPtr,visualTile(row,col)) = Boolean.True Then
    	Return Boolean.True
    EndIf    
  else    
    logPtr->debug("Unseedable tile!")
  end If
  Return Boolean.False
End Function

Function Map.harvest ( row As Integer, col As Integer ) As Crop Ptr
	If _soilData(row,col) <> NULL Then
		Dim As Crop Ptr harvestedCrop = _soilData(row,col)->harvest() 
		If harvestedCrop <> NULL Then
			Delete _soilData(row,col)
			_soilData(row,col) = NULL
			visualTile(row,col) = SOIL_LIGHT
		EndIf
		Return harvestedCrop
	End If
	Return NULL
End Function

Function Map.walkAble(row as integer, col as integer) as Boolean
  if row >= 1 and col >= 1 then
    if row <= (height - 2) and col <= (width - 2) then
      return Boolean.True
    end if
  end if
  return Boolean.False
End Function

Sub Map.update()
  for row as integer = 0 to (height - 1)
    for col as integer = 0 to (width - 1)
      if _soilData(row,col) <> NULL then
	_soilData(row,col)->newDay(visualTile(row,col))
      end if
    next col
  next row
End Sub

Function Map.getTileInfo(row As Integer, col As integer) As String
	If _soilData(row,col) <> NULL Then
		Return _soilData(row,col)->getStateString() 
	Else
		Select Case typeMap(row,col)
			Case TileType.Soil:
				Return "Fallow piece of land."
			Case TileType.Fence:
				Return "a Fence."
			Case TileType.Water:
				Return "Water."
			Case TileType.Building:
				Return "Building."
			Case Else:
				Return "Unknown TileType, should not happen!"
		End Select	
	EndIf
End Function

Type Budget
	Private:
		amount As Integer = 1000
	Public:
		Declare Constructor ()
		Declare Constructor ( startAmount As Integer )
		Declare Sub addAmount ( _amount As Integer )
		Declare Sub substractAmount ( _amount As Integer )
		Declare Function canSpend ( _amount As Integer ) As Boolean
		Declare Function getBalance () As Integer	
End Type

Constructor Budget ()
End Constructor

Constructor Budget ( startAmount As Integer )
	amount = startAmount
End Constructor

Sub Budget.addAmount ( _amount As Integer )
	amount += _amount
End Sub

Sub Budget.subStractAmount ( _amount As Integer )
	amount -= _amount
End Sub

Function Budget.canSpend ( _amount As Integer ) As Boolean
	If (amount - _amount) >= 0 Then
		Return Boolean.True
	EndIf
	Return Boolean.False
End Function

Function Budget.getBalance() as Integer
	return amount
End Function
	
Type Engine	
  private:
    _logger As Logger
    _budget As Budget = Budget(1000)
    myMap as Map
    rowOffset As integer = 0
    colOffset as integer = 0
    cursorRow as integer = 1
    cursorCol as integer = 1
    maxRow as integer
    maxCol as integer
    landHeight as integer
    landWidth as integer
    days as integer = 0
    ' Helpers for Engine
    Declare Sub WriteLine (row as Integer,text as String)
    Declare Sub drawStatusBar ()
  public:
    Declare Constructor (w as integer, h as integer, rOff as integer, cOff as integer)
    Declare Sub moveCursor (rowMove as integer, colMove as integer)
    Declare Sub drawCursor ()
    Declare Sub _drawMap ()
    Declare Sub plowTile ()
    Declare Sub plantCrop (cropToPlant as Crop ptr)
    Declare Sub harvest ()
    Declare Sub startNewDay ()
    Declare Sub init ()
    Declare Sub getTileInfo ()    
End Type

Constructor Engine(w as integer, h as integer, rOff as integer, cOff as integer)
	myMap = Map(w,h,@_logger)
	landHeight = h
	landWidth = w
	rowOffset = rOff
	colOffset = cOff
End Constructor

Sub Engine.init()
	dim h as integer
	dim w as integer
	screeninfo w,h
	maxRow = h \ 16
	maxCol = w \ 8
	' Set the screen to use a certain character size.
	width maxCol, maxRow
	rowOffset = (maxRow - landHeight) \ 2
	colOffset = (maxCol - landWidth) \ 2
	_drawMap()
End Sub

Sub Engine.moveCursor(rowMove as integer,colMove as integer)
	Dim newCursorRow as integer = (cursorRow + rowMove)
	Dim newCursorCol as integer = (cursorCol + colMove)
	if myMap.walkable(newCursorRow,newCursorCol) = Boolean.True then
	cursorRow = newCursorRow
	cursorCol = newCursorCol
	_drawMap()
	drawCursor()
	end if
End Sub

Sub Engine.drawCursor()
	Dim cursorTile as Tile = Tile(0,2,219)
	cursorTile.drawMe(1 + (rowOffset + cursorRow),1 + (colOffset + cursorCol))
End Sub

Sub Engine._drawMap()
	myMap.drawMap(rowOffset,colOffset)
End Sub

Sub Engine.plowTile()  
	myMap.plow(cursorRow,cursorCol)
	myMap.drawMap(rowOffset,colOffset)
End Sub

Sub Engine.plantCrop(cropToPlant as Crop ptr)
	Dim _seedPrice As Integer = cropToPlant->getSeedPrice()
	If _budget.canSpend(_seedPrice) = Boolean.True Then
		If myMap.seed(cursorRow,cursorCol,cropToPlant) = Boolean.True Then
 			_budget.subStractAmount(_seedPrice)
		EndIf
	Else
		_logger.debug("Can't afford this seed.")
	EndIf	
	myMap.drawMap(rowOffset,colOffset)
End Sub

Sub Engine.harvest()
	Dim As Crop Ptr tileCrop = myMap.harvest(cursorRow,cursorCol)
	If tileCrop <> NULL Then
		Dim As Integer price = tileCrop->getHarvestPrice()
		_budget.addAmount(price)
	Else
		_logger.debug("No crop to harvest.")
	EndIf
End Sub

Sub Engine.startNewDay()
	days += 1
	drawStatusBar()
	myMap.update()
	myMap.drawMap(rowOffset,colOffset)
	drawCursor()
End Sub

Sub Engine.getTileInfo()
	Dim As String info = "Tile #@ " & cursorRow & "," & cursorCol & ": " & myMap.getTileInfo(cursorRow,cursorCol)
	_logger.debug(info)
End Sub

Sub Engine.drawStatusBar()
	Dim as Integer balance = _budget.getBalance()
	Dim as String statusText = (" day: " & days & " || $" & balance)
	writeLine (1, statusText)
End Sub

Sub Engine.writeLine(row as Integer, text as String)
	locate row,1
	color 7,1
	print string(maxCol," ")
	locate row,1
	print text	
End Sub
	
Screen 18

' Initialize Engine
Dim e as Engine = Engine(20,10,1,2)
e.init()

'===========
' Main Loop
'===========
Dim ticks as integer = 0
Dim k as string
Do    
  k = Inkey$
  Select Case k
    case "w":
		e.moveCursor(-1,-0)
    case "a":
		e.moveCursor(0,-1)
    case "s":
		e.moveCursor(1,0)
    case "d":
		e.moveCursor(0,1)
    case "p":
		e.plowTile()
    case "c":
		e.plantCrop(@CROP_CLOVER)
    case "h":
		e.harvest()
  	Case "i":
  		e.getTileInfo()
  End Select
  sleep 10,1
  ticks += 1
  if ticks > TICKS_PER_DAY then
    e.startNewDay()
    ticks = 0
  end if
Loop While k <> chr$(27)

'------
' Exit
'------
System

'==============
' DATA SECTION
'
' Each line represents a crop
' SP = Price of seed ($).
' HP = Price when harvested ($).
' DG = Days to grow (#days).
' DH = Days to harvest after fullgrown (#days).
'	   
'    SP  HP  DG  DH
'==================
Data 10, 15, 10, 10 
