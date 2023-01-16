' -------- DECLARACIONES ---------


Type gba_io_writeCallack_t As Sub(addr As uint32_t,value As uint16_t)

Type gba_io_register_t ' sizeof=12
    As uint16_t value 
    As uint16_t readMask 
    As uint16_t writeMask 
    As gba_io_writeCallack_t Ptr writeCallback 
End Type 

Declare Sub gba_io_reset() 
Declare Function gba_io_read8(addr As uint32_t) As uint8_t 
Declare Function gba_io_read16(addr As uint32_t) As uint16_t 
Declare Function gba_io_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_io_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_io_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_io_write32(addr As uINT32_T , value As uint32_t) 
Declare Function gba_io_getRegister(addr As uint32_t) As gba_io_register_t ptr

Declare Sub gba_setInterruptFlag(flag As uint16_t) 
Declare Sub gba_writeToIf (addr As uint32_t , flag As uint16_t) 


' FRONTEND
Declare Sub frontend_close() 
Declare Sub frontend_frame( buffer As uint32_t Ptr) 
Declare Function frontend_init() As Integer 



' BIOS
Declare Sub gba_bios_init(buffer As uint32_t Ptr) 
Declare Function gba_bios_read8(addr As uint32_t) As uint8_t 
Declare Function gba_bios_read16(addr As uint32_t) As uint16_t 
Declare Function gba_bios_read32(addr As uINT32_T) As uint32_t 


' BUS
Declare Function gba_bus_read8(addr As uint32_t) As uint8_t 
Declare Function gba_bus_read16(addr As uint32_t) As uint16_t 
Declare Function gba_bus_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_bus_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_bus_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_bus_write32(addr As uINT32_T , value As uint32_t) 


' CARTRIDGE
Declare Sub gba_cartridge_init(buffer As uint32_t Ptr , size As size_t) 
Declare Function gba_cartridge_rom_read8(addr As uint32_t) As uint8_t 
Declare Function gba_cartridge_rom_read16(addr As uint32_t) As uint16_t 
Declare Function gba_cartridge_rom_read32(addr As uINT32_T) As uint32_t 
Declare Function gba_cartridge_sram_read8(addr As uint32_t) As uint8_t 
Declare Function gba_cartridge_sram_read16(addr As uint32_t) As uint16_t 
Declare Function gba_cartridge_sram_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_cartridge_sram_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_cartridge_sram_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_cartridge_sram_write32(addr As uINT32_T , value As uint32_t) 


' CPU
Declare Sub gba_cpu_init() 
Declare Sub gba_cpu_reset(skipBoot As BOOL) 
Declare Sub gba_cpu_cycle() 


' DMA
Declare Sub gba_dma_reset() 
Declare Function gba_dma_cycle() As BOOL 
Declare Sub gba_dma_writeCallback_cntH0(addr As uint32_t , value As uint16_t) 
Declare Sub gba_dma_writeCallback_cntH1(addr As uint32_t , value As uint16_t) 
Declare Sub gba_dma_writeCallback_cntH2(addr As uint32_t , value As uint16_t) 
Declare Sub gba_dma_writeCallback_cntH3(addr As uint32_t , value As uint16_t) 
Declare Sub gba_dma_onVblank() 
Declare Sub gba_dma_onHblank() 

' EWRAM
Declare Sub gba_ewram_reset() 
Declare Function gba_ewram_read8(addr As uint32_t) As uint8_t 
Declare Function gba_ewram_read16(addr As uint32_t) As uint16_t 
Declare Function gba_ewram_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_ewram_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_ewram_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_ewram_write32(addr As uINT32_T , value As uint32_t) 

' IWRAM
Declare Sub gba_iwram_reset() 
Declare Function gba_iwram_read8(addr As uint32_t) As uint8_t 
Declare Function gba_iwram_read16(addr As uint32_t) As uint16_t 
Declare Function gba_iwram_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_iwram_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_iwram_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_iwram_write32(addr As uINT32_T , value As uint32_t) 

' KEYPAD
Declare Sub gba_keypad_update( pressedKeys As BOOL Ptr) 
Declare Sub gba_keypad_cycle() 

' PPU
Declare Sub gba_ppu_reset() 
Declare Sub gba_ppu_cycle() 
Declare Function gba_ppu_palette_read8(addr As uint32_t) As uint8_t 
Declare Function gba_ppu_palette_read16(addr As uint32_t) As uint16_t 
Declare Function gba_ppu_palette_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_ppu_palette_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_ppu_palette_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_ppu_palette_write32(addr As uINT32_T , value As uint32_t) 
Declare Function gba_ppu_vram_read8(addr As uint32_t) As uint8_t 
Declare Function gba_ppu_vram_read16(addr As uint32_t) As uint16_t 
Declare Function gba_ppu_vram_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_ppu_vram_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_ppu_vram_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_ppu_vram_write32(addr As uINT32_T , value As uint32_t) 
Declare Function gba_ppu_oam_read8(addr As uint32_t) As uint8_t 
Declare Function gba_ppu_oam_read16(addr As uint32_t) As uint16_t 
Declare Function gba_ppu_oam_read32(addr As uINT32_T) As uint32_t 
Declare Sub gba_ppu_oam_write8(addr As uint32_t , value As uint8_t) 
Declare Sub gba_ppu_oam_write16(addr As uint32_t , value As uint16_t) 
Declare Sub gba_ppu_oam_write32(addr As uINT32_T , value As uint32_t) 
Declare Sub gba_ppu_drawLine() 
Declare Sub gba_ppu_onVblank() 
Declare sub gba_ppu_onHblank() 
Declare sub gba_onFrame()

' TIMER
Declare Sub gba_timer_reset() 
Declare Sub gba_timer_cycle() 
Declare Sub gba_timer_writeCallback_channel0_reload(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel0_control(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel1_reload(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel1_control(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel2_reload(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel2_control(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel3_reload(addr As uint32_t , value As uint16_t) 
Declare Sub gba_timer_writeCallback_channel3_control(addr As uint32_t , value As uint16_t) 




Sub UNUSED(dato As Integer)
	'Print "Instruccion ";Hex(dato,16);" no usado"
End Sub



Function leeBIT(valor As uint32_t, rot As uint8_t) As uint8_t
	Return IIf( (valor And (1 Shl rot)) <> 0, 1, 0)
End Function
