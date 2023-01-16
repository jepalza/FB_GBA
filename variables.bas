' -------- VARIABLES ---------

static shared DEB As Integer=0 ' 0 nada, 1 solo BIOS y CART, 2 incluye llamadas, 3 hace pausa en llamadas (pero no en memoria)
Static Shared salir As Integer=0

#Undef  TRUE
#define TRUE 1

#Undef  FALSE
#define FALSE 0

' VARIOS
'#Define SIZE_T integer
#Define  uint8_t UByte
#Define uint16_t UShort
#Define uint32_t ULong
#Define uint64_t ULongInt

#Define  int8_t Byte
#Define int16_t Short
#Define int32_t Long
#Define int64_t LongInt


' no es necesario, lo define SDL , pero para mi gusto, yo lo hago
#Undef  BOOL
#Define BOOL ubyte

' modos graficos principales 0,1,2,3 y 4
#define GBA_SCREEN_WIDTH  240
#define GBA_SCREEN_HEIGHT 160

' modo grafico 5 especial 32k colores
#Define GBA_MODE5_WIDTH  160
#Define GBA_MODE5_HEIGHT 128


' MEMORIAS
'BIOS  = 0x00000000
#Define GBA_BIOS_FILE_SIZE &h4000 '16384
static shared As uint8_t bios(GBA_BIOS_FILE_SIZE-1) ' almacen para la BIOS de la GBA

'ROM   = 0x08000000
#define GBA_MAX_ROM_FILE_SIZE &h2000000 '33554432 (32mb)
static shared As uint8_t rom(GBA_MAX_ROM_FILE_SIZE-1) ' juegos hasta 32mb

'SRAM   = 0x08000000
#define GBA_MAX_SRAM_FILE_SIZE &h8000 '(32k) (la docu oficial dice de un max. de 64)
static shared As uint8_t gba_sram_buffer(GBA_MAX_SRAM_FILE_SIZE-1) ' juegos hasta 32mb

'PALTT = 0x05000000
#Define GBA_PALETTE_SIZE &h400 '1024
static shared As uint8_t gba_ppu_palette(GBA_PALETTE_SIZE-1) 

'VRAM  = 0x06000000 (+0x10000 -> text-mode BG) (+0x14000 -> bitmap-mode BG)
#define GBA_VRAM_SIZE &h18000 '98304
static shared As uint8_t gba_ppu_vram(GBA_VRAM_SIZE-1) 

'OAM   = 0x07000000
#define GBA_OAM_SIZE &h400 '1024
static shared As uint8_t gba_ppu_oam(GBA_OAM_SIZE-1) 

'EWRAM = 0x02000000
#Define GBA_EWRAM_SIZE &h40000 '262144 
static shared As uint8_t gba_ewram_buffer(GBA_EWRAM_SIZE-1) 

'IWRAM = 0x03000000
#Define GBA_IWRAM_SIZE &h8000 '32768
static shared As uint8_t gba_iwram_buffer(GBA_IWRAM_SIZE-1) 

'REG_BASE = 0x04000000
#Define GBA_REG_BASE  &h04000000

#Define UINT32_MAX &hFFFFFFFF


' globales para uso general
static shared As String sa,sb,sc,sd
static Shared As Integer a,b,c,d,e,f,posfil
