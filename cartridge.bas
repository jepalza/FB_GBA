
static shared As Any Ptr gba_cartridge_rom_buffer 
static shared As size_t gba_cartridge_rom_size 
static shared As Any ptr gba_cartridge_sram_buffer   
static shared As size_t gba_cartridge_sram_size 

static shared As uint32_t gba_cartridge_rom_addressMask8 
static shared As uint32_t gba_cartridge_rom_addressMask16 
static shared As uint32_t gba_cartridge_rom_addressMask32 


Sub gba_cartridge_init(buffer As uint32_t Ptr , size As size_t) 
    gba_cartridge_rom_buffer  = buffer 
    gba_cartridge_rom_size    = size 
    gba_cartridge_sram_buffer = NULL 
    gba_cartridge_sram_size   = 0 
    gba_cartridge_rom_addressMask8  = size - 1 ' 1FFFFFF
    gba_cartridge_rom_addressMask16 = gba_cartridge_rom_addressMask8 And NOT(&h00000001) ' 1FFFFFE
    gba_cartridge_rom_addressMask32 = gba_cartridge_rom_addressMask16 And NOT(&h00000002) ' 1FFFFFC 
End Sub

Function gba_cartridge_rom_read8(addr As uint32_t) As uint8_t 
    return ACCESS_8(gba_cartridge_rom_buffer, addr And gba_cartridge_rom_addressMask8) 
End Function

Function gba_cartridge_rom_read16(addr As uint32_t) As uint16_t 
    return ACCESS_16(gba_cartridge_rom_buffer, addr And gba_cartridge_rom_addressMask16) 
End Function

Function gba_cartridge_rom_read32(addr As uint32_t) As uint32_t 
	'If DEB>0 Then Print "CART:";Hex(addr,8),Hex(ACCESS_32(gba_cartridge_rom_buffer, addr And gba_cartridge_rom_addressMask32) ,8) :Sleep
    Return ACCESS_32(gba_cartridge_rom_buffer, addr And gba_cartridge_rom_addressMask32) 
End Function





' SRAM READ
Function gba_cartridge_sram_read8(addr As uint32_t) As uint8_t
    return gba_sram_read8(addr) 
End Function

Function gba_cartridge_sram_read16(addr As uint32_t) As uint16_t 
    return gba_sram_read16(addr)
End Function

Function gba_cartridge_sram_read32(addr As uint32_t) As uint32_t 
    return gba_sram_read32(addr)
End Function


' SRAM WRITE
Sub gba_cartridge_sram_write8(addr As uint32_t , value As uint8_t) 
	gba_sram_write8(addr,value)
End Sub

Sub gba_cartridge_sram_write16(addr As uint32_t , value As uint16_t) 
	gba_sram_write16(addr,value)
End Sub

Sub gba_cartridge_sram_write32(addr As uint32_t , value As uint32_t) 
	gba_sram_write32(addr,value)
End Sub
