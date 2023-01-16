' I/O
static shared As gba_io_register_t gba_io_registers(511) 
static shared As gba_io_register_t gba_io_register_internalMemoryControl_low 
static shared As gba_io_register_t gba_io_register_internalMemoryControl_high 
static shared As gba_io_register_t gba_io_nullRegister 

Declare Sub gba_io_initRegister(addr As uint32_t , initialValue As uint16_t ,  writeCallback As gba_io_writeCallack_t , readMask As uint16_t , writeMask As uint16_t) 

Sub gba_io_reset() 
    memset(@gba_io_registers(0), 0, sizeof(gba_io_registers)*512) ' sizeof=6144

    gba_io_register_internalMemoryControl_low.value          = &h0000 
    gba_io_register_internalMemoryControl_low.readMask       = &hffff 
    gba_io_register_internalMemoryControl_low.writeMask      = &hffff 
    gba_io_register_internalMemoryControl_low.writeCallback  = NULL 
    
    gba_io_register_internalMemoryControl_high.value         = &h0000 
    gba_io_register_internalMemoryControl_high.readMask      = &hffff 
    gba_io_register_internalMemoryControl_high.writeMask     = &hffff 
    gba_io_register_internalMemoryControl_high.writeCallback = NULL 

    gba_io_nullRegister.value         = &h0000 
    gba_io_nullRegister.readMask      = &h0000 
    gba_io_nullRegister.writeMask     = &hffff 
    gba_io_nullRegister.writeCallback = NULL 
    
    ' GBA_REG_BASE = &h04000000
    gba_io_initRegister(&h04000000, &h0000, NULL, &hffff, &hffff)  ' DISPCNT
    gba_io_initRegister(&h04000002, &h0000, NULL, &hffff, &hffff)  ' GREENSWP
    gba_io_initRegister(&h04000004, &h0000, NULL, &hff3f, &hff3f)  ' DISPSTAT
    gba_io_initRegister(&h04000006, &h0000, NULL, &hffff, &h0000)  ' VCOUNT
    gba_io_initRegister(&h04000008, &h0000, NULL, &hffff, &hffff)  ' BG0CNT
    gba_io_initRegister(&h0400000a, &h0000, NULL, &hffff, &hffff)  ' BG1CNT
    gba_io_initRegister(&h0400000c, &h0000, NULL, &hffff, &hffff)  ' BG2CNT
    gba_io_initRegister(&h0400000e, &h0000, NULL, &hffff, &hffff)  ' BG3CNT
    gba_io_initRegister(&h04000010, &h0000, NULL, &h0000, &h01ff)  ' BG0HOFS
    gba_io_initRegister(&h04000012, &h0000, NULL, &h0000, &h01ff)  ' BG0VOFS
    gba_io_initRegister(&h04000014, &h0000, NULL, &h0000, &h01ff)  ' BG1HOFS
    gba_io_initRegister(&h04000016, &h0000, NULL, &h0000, &h01ff)  ' BG1VOFS
    gba_io_initRegister(&h04000018, &h0000, NULL, &h0000, &h01ff)  ' BG2HOFS
    gba_io_initRegister(&h0400001a, &h0000, NULL, &h0000, &h01ff)  ' BG2VOFS
    gba_io_initRegister(&h0400001c, &h0000, NULL, &h0000, &h01ff)  ' BG3HOFS
    gba_io_initRegister(&h0400001e, &h0000, NULL, &h0000, &h01ff)  ' BG3VOFS
    gba_io_initRegister(&h04000020, &h0000, NULL, &h0000, &hffff)  ' BG2PA
    gba_io_initRegister(&h04000022, &h0000, NULL, &h0000, &hffff)  ' BG2PB
    gba_io_initRegister(&h04000024, &h0000, NULL, &h0000, &hffff)  ' BG2PC
    gba_io_initRegister(&h04000026, &h0000, NULL, &h0000, &hffff)  ' BG2PD
    gba_io_initRegister(&h04000028, &h0000, NULL, &h0000, &hffff)  ' BG2X_L
    gba_io_initRegister(&h0400002a, &h0000, NULL, &h0000, &hffff)  ' BG2X_H
    gba_io_initRegister(&h0400002c, &h0000, NULL, &h0000, &hffff)  ' BG2Y_L
    gba_io_initRegister(&h0400002e, &h0000, NULL, &h0000, &hffff)  ' BG2Y_H
    gba_io_initRegister(&h04000030, &h0000, NULL, &h0000, &hffff)  ' BG3PA
    gba_io_initRegister(&h04000032, &h0000, NULL, &h0000, &hffff)  ' BG3PB
    gba_io_initRegister(&h04000034, &h0000, NULL, &h0000, &hffff)  ' BG3PC
    gba_io_initRegister(&h04000036, &h0000, NULL, &h0000, &hffff)  ' BG3PD
    gba_io_initRegister(&h04000038, &h0000, NULL, &h0000, &hffff)  ' BG3X_L
    gba_io_initRegister(&h0400003a, &h0000, NULL, &h0000, &hffff)  ' BG3X_H
    gba_io_initRegister(&h0400003c, &h0000, NULL, &h0000, &hffff)  ' BG3Y_L
    gba_io_initRegister(&h0400003e, &h0000, NULL, &h0000, &hffff)  ' BG3Y_H
    gba_io_initRegister(&h04000040, &h0000, NULL, &h0000, &hffff)  ' WIN0H
    gba_io_initRegister(&h04000042, &h0000, NULL, &h0000, &hffff)  ' WIN1H
    gba_io_initRegister(&h04000044, &h0000, NULL, &h0000, &hffff)  ' WIN0V
    gba_io_initRegister(&h04000046, &h0000, NULL, &h0000, &hffff)  ' WIN1V
    gba_io_initRegister(&h04000048, &h0000, NULL, &hffff, &hffff)  ' WININ
    gba_io_initRegister(&h0400004a, &h0000, NULL, &hffff, &hffff)  ' WINOUT
    gba_io_initRegister(&h0400004c, &h0000, NULL, &h0000, &hffff)  ' MOSAIC
    gba_io_initRegister(&h04000050, &h0000, NULL, &hffff, &hffff)  ' BLDCNT
    gba_io_initRegister(&h04000052, &h0000, NULL, &hffff, &hffff)  ' BLDALPHA
    gba_io_initRegister(&h04000054, &h0000, NULL, &h0000, &hffff)  ' BLDY
    gba_io_initRegister(&h040000b0, &h0000, NULL, &h0000, &hffff)  ' DMA0SAD_L
    gba_io_initRegister(&h040000b2, &h0000, NULL, &h0000, &h07ff)  ' DMA0SAD_H
    gba_io_initRegister(&h040000b4, &h0000, NULL, &h0000, &hffff)  ' DMA0DAD_L
    gba_io_initRegister(&h040000b6, &h0000, NULL, &h0000, &h07ff)  ' DMA0DAD_H
    gba_io_initRegister(&h040000b8, &h0000, NULL, &h0000, &h3fff)  ' DMA0CNT_L
    gba_io_initRegister(&h040000ba, &h0000, @gba_dma_writeCallback_cntH0, &hffe0, &hffe0)  ' DMA0CNT_H
    gba_io_initRegister(&h040000bc, &h0000, NULL, &h0000, &hffff)  ' DMA1SAD_L
    gba_io_initRegister(&h040000be, &h0000, NULL, &h0000, &h07ff)  ' DMA1SAD_H
    gba_io_initRegister(&h040000c0, &h0000, NULL, &h0000, &hffff)  ' DMA1DAD_L
    gba_io_initRegister(&h040000c2, &h0000, NULL, &h0000, &h07ff)  ' DMA1DAD_H
    gba_io_initRegister(&h040000c4, &h0000, NULL, &h0000, &h3fff)  ' DMA1CNT_L
    gba_io_initRegister(&h040000c6, &h0000, @gba_dma_writeCallback_cntH1, &hffe0, &hffe0)  ' DMA1CNT_H
    gba_io_initRegister(&h040000c8, &h0000, NULL, &h0000, &hffff)  ' DMA2SAD_L
    gba_io_initRegister(&h040000ca, &h0000, NULL, &h0000, &h07ff)  ' DMA2SAD_H
    gba_io_initRegister(&h040000cc, &h0000, NULL, &h0000, &hffff)  ' DMA2DAD_L
    gba_io_initRegister(&h040000ce, &h0000, NULL, &h0000, &h07ff)  ' DMA2DAD_H
    gba_io_initRegister(&h040000d0, &h0000, NULL, &h0000, &h3fff)  ' DMA2CNT_L
    gba_io_initRegister(&h040000d2, &h0000, @gba_dma_writeCallback_cntH2, &hffe0, &hffe0)  ' DMA2CNT_H
    gba_io_initRegister(&h040000d4, &h0000, NULL, &h0000, &hffff)  ' DMA3SAD_L
    gba_io_initRegister(&h040000d6, &h0000, NULL, &h0000, &h0fff)  ' DMA3SAD_H
    gba_io_initRegister(&h040000d8, &h0000, NULL, &h0000, &hffff)  ' DMA3DAD_L
    gba_io_initRegister(&h040000da, &h0000, NULL, &h0000, &h0fff)  ' DMA3DAD_H
    gba_io_initRegister(&h040000dc, &h0000, NULL, &h0000, &hffff)  ' DMA3CNT_L
    gba_io_initRegister(&h040000de, &h0000, @gba_dma_writeCallback_cntH3,              &hffe0, &hffe0)  ' DMA3CNT_H
    gba_io_initRegister(&h04000100, &h0000, @gba_timer_writeCallback_channel0_reload,  &hffff, &h0000)  ' TM0D
    gba_io_initRegister(&h04000102, &h0000, @gba_timer_writeCallback_channel0_control, &h00c3, &h00c3)  ' TM0CNT
    gba_io_initRegister(&h04000104, &h0000, @gba_timer_writeCallback_channel1_reload,  &hffff, &h0000)  ' TM1D
    gba_io_initRegister(&h04000106, &h0000, @gba_timer_writeCallback_channel1_control, &h00c3, &h00c3)  ' TM1CNT
    gba_io_initRegister(&h04000108, &h0000, @gba_timer_writeCallback_channel2_reload,  &hffff, &h0000)  ' TM2D
    gba_io_initRegister(&h0400010a, &h0000, @gba_timer_writeCallback_channel2_control, &h00c3, &h00c3)  ' TM2CNT
    gba_io_initRegister(&h0400010c, &h0000, @gba_timer_writeCallback_channel3_reload,  &hffff, &h0000)  ' TM3D
    gba_io_initRegister(&h0400010e, &h0000, @gba_timer_writeCallback_channel3_control, &h00c3, &h00c3)  ' TM3CNT
    gba_io_initRegister(&h04000130, &hffff, NULL, &h03ff, &h0000)  ' KEYINPUT
    gba_io_initRegister(&h04000132, &h0000, NULL, &hc3ff, &hc3ff)  ' KEYCNT
    gba_io_initRegister(&h04000200, &h0000, NULL, &h3fff, &h3fff)  ' IE
    gba_io_initRegister(&h04000202, &h0000, @gba_writeToIF, &h3fff, &h0000)  ' IF
    gba_io_initRegister(&h04000208, &h0000, NULL, &h0001, &h0001)  ' IME
End Sub

Function gba_io_read8(addr As uint32_t) As uint8_t 
    Dim As uint16_t value = gba_io_read16(addr) 

    If (addr And 1) Then 
        return value Shr 8 
    else      
        return value 
    End If
 
End Function

Function gba_io_read16(addr As uint32_t) As uint16_t 
    return gba_io_getRegister(addr)->value 
End Function

Function gba_io_read32(addr As uINT32_T) As uint32_t 
    return gba_io_read16(addr) Or (gba_io_read16(addr + 2) Shl 16) 
End Function

Sub gba_io_write8(addr As uint32_t , value As uint8_t) 
    Dim As uint16_t v = gba_io_read16(addr) 

    If (addr And 1) Then 
        v And= &h00ff 
        v Or= value Shl 8 
    else    
        v And= &hff00 
        v Or= value 
    End If

    gba_io_write16(addr, v) 
End Sub

Sub gba_io_write16(addr As uint32_t , value As uint16_t) 
    Dim As gba_io_register_t Ptr reg = gba_io_getRegister(addr) 

    reg->value And= Not(reg->writeMask) 
    reg->value Or= value And reg->writeMask 

    If (reg->writeCallback) Then 
       Dim proceso As Sub(As uint32_t, As uint16_t)=reg->writeCallback
       proceso(addr, value) 
    End If
 
End Sub

Sub gba_io_write32(addr As uint32_t , value As uint32_t) 
    gba_io_write16(addr, value) 
    gba_io_write16(addr + 2, value Shr 16) 
End Sub


Function gba_io_getRegister(addr As uint32_t) As gba_io_register_t Ptr

    If ((addr And &h0f00fffe) = &h04000800) Then 
        return @gba_io_register_internalMemoryControl_low 
    ElseIf ((addr And &h0f00fffe) = &h04000802) Then              
        return @gba_io_register_internalMemoryControl_high
    ElseIf ((addr And &h0ffffffe) < &h04000400) Then 
        return @gba_io_registers((addr And &h000003ff) Shr 1) 
    else   
        return @gba_io_nullRegister 
    End If
 
End Function

Sub gba_io_initRegister(addr As uint32_t , initialValue As uint16_t ,  writeCallback As gba_io_writeCallack_t , readMask As uint16_t , writeMask As uint16_t) 
    Dim As gba_io_register_t Ptr reg = gba_io_getRegister(addr) 

    If (reg <> @gba_io_nullRegister) Then 
        reg->value = initialValue 
        reg->writeCallback = writeCallback 
        reg->readMask = readMask 
        reg->writeMask = writeMask 
    End If
 
End Sub







' -------------------------------------------------------
' 
Function po2_ceil(initialValue As Long) As long 
   Dim As Integer hammingWeight = 0 
   Dim As Integer lastBitTo1 = 0 
	Dim As Integer i,j,shift,bit_

    for i = 0 To 3 ' ojo con esto --> SizeOf(long) que pueden ser 4 u 8         
        for j = 0 To 7         
            shift = i * 8 + j 
            bit_ = (initialValue Shr shift) And 1 

            If (bit_) Then 
                lastBitTo1 = shift 
                hammingWeight+=1  
            End If
        Next
    Next

    If (hammingWeight = 1) Then 
        return initialValue 
    else 
        return 1  Shl (lastBitTo1 + 1) 
    End If
 
End Function







' KEYPAD
Sub gba_keypad_update( pressedKeys As BOOL Ptr) 
    Dim As uint16_t value = &hffff 

    for i As integer= 0 To 9         
        If (pressedKeys[i]) Then 
            value And=  NOT(1 Shl i) 
        End If
    Next

    Dim As gba_io_register_t Ptr keyinput = gba_io_getRegister(GBA_REG_BASE+&h130)
    keyinput->value = value 
End Sub

Sub gba_keypad_cycle() 
    Dim As gba_io_register_t Ptr keyinput = gba_io_getRegister(GBA_REG_BASE+&h130) 
    Dim As gba_io_register_t Ptr keycnt   = gba_io_getRegister(GBA_REG_BASE+&h132) 

    If (keycnt->value And (1 Shl 14)) Then 
        If (keycnt->value And (1 Shl 15)) Then 
            If ((keycnt->value And NOT(keyinput->value) And &h03ff) = (keycnt->value And &h03ff)) Then 
                gba_setInterruptFlag(1 Shl 12) 
            Else
	            If (keycnt->value And NOT(keyinput->value) And &h03ff) Then 
	                gba_setInterruptFlag(1 Shl 12)
	            End If
            End If
        End If
    End If
 
End Sub








' BUS
Function gba_bus_read8(addr As uint32_t) As uint8_t 
    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	case &h00,&h01  ' BIOS
        return gba_bios_read8(addr) 

    	Case &h02  ' EWRAM
        return gba_ewram_read8(addr) 

    	Case &h03  ' IWRAM
        return gba_iwram_read8(addr) 

    	case &h04  ' IO
        return gba_io_read8(addr) 

    	case &h05  ' Palette
        return gba_ppu_palette_read8(addr) 

    	case &h06  ' VRAM
        return gba_ppu_vram_read8(addr) 

    	case &h07  ' OAM
        return gba_ppu_oam_read8(addr) 

    	case &h08, &h09  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read8(addr) 

    	case &h0a, &h0b ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read8(addr) 

    	Case &h0c, &h0d  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read8(addr) 

    	Case &h0e, &h0f  ' Game Pak SRAM
        return gba_cartridge_sram_read8(addr) 
    End Select

    return &h00 
End Function

Function gba_bus_read16(addr As uint32_t) As uint16_t 
    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	case &h00,&h01  ' BIOS
        return gba_bios_read16(addr) 

    	Case &h02  ' EWRAM
        return gba_ewram_read16(addr) 

    	Case &h03  ' IWRAM
        return gba_iwram_read16(addr) 

    	Case &h04  ' IO
        return gba_io_read16(addr) 

    	Case &h05  ' Palette
        return gba_ppu_palette_read16(addr) 

    	Case &h06  ' VRAM
        return gba_ppu_vram_read16(addr) 

    	Case &h07  ' OAM
        return gba_ppu_oam_read16(addr) 

    	case &h08, &h09  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read16(addr) 

    	Case &h0a, &h0b  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read16(addr) 

    	Case &h0c, &h0d ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read16(addr) 

    	Case &h0e, &h0f  ' Game Pak SRAM (0e principal, 0f mirror)
        return gba_cartridge_sram_read16(addr) 
    End Select

    return &h0000 
End Function

Function gba_bus_read32(addr As uint32_t) As uint32_t 

    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	case &h00, &h01  ' BIOS
        return gba_bios_read32(addr) 

    	Case &h02  ' EWRAM
        return gba_ewram_read32(addr) 

    	case &h03  ' IWRAM
        return gba_iwram_read32(addr) 

    	case &h04  ' IO
        return gba_io_read32(addr) 

    	case &h05  ' Palette
        return gba_ppu_palette_read32(addr) 

    	case &h06  ' VRAM
        return gba_ppu_vram_read32(addr) 

    	case &h07  ' OAM
        return gba_ppu_oam_read32(addr) 

    	case &h08, &h09  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read32(addr) 

    	case &h0a, &h0b  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read32(addr) 

    	case &h0c, &h0d  ' Game Pak ROM Wait State 0
        return gba_cartridge_rom_read32(addr) 

    	case &h0e, &h0f  ' Game Pak SRAM
        return gba_cartridge_sram_read32(addr) 
    End Select

    return &h00000000 
End Function

Sub gba_bus_write8(addr As uint32_t , value As uint8_t) 
    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	Case &h02  ' EWRAM
        gba_ewram_write8(addr, value) 

    	Case &h03  ' IWRAM
        gba_iwram_write8(addr, value) 

    	Case &h04  ' IO
        gba_io_write8(addr, value) 

    	Case &h05  ' Palette
        gba_ppu_palette_write8(addr, value) 

    	Case &h06  ' VRAM
        gba_ppu_vram_write8(addr, value) 

    	Case &h07  ' OAM
        gba_ppu_oam_write8(addr, value) 

    	case &h0e, &h0f  ' Game Pak SRAM
        gba_cartridge_sram_write8(addr, value) 
    End Select

End Sub

Sub gba_bus_write16(addr As uint32_t , value As uint16_t) 
    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	Case &h02  ' EWRAM
        gba_ewram_write16(addr, value) 

    	case &h03  ' IWRAM
        gba_iwram_write16(addr, value) 

    	case &h04  ' IO
        gba_io_write16(addr, value) 

    	case &h05  ' Palette
        gba_ppu_palette_write16(addr, value) 

    	case &h06  ' VRAM
        gba_ppu_vram_write16(addr, value) 

    	case &h07  ' OAM
        gba_ppu_oam_write16(addr, value) 

    	case &h0e, &h0f  ' Game Pak SRAM
        gba_cartridge_sram_write16(addr, value) 
    End Select

End Sub

Sub gba_bus_write32(addr As uint32_t , value As uint32_t) 
    Select Case As Const   ((addr And &h0f000000) Shr 24)  
    	Case &h02  ' EWRAM
        gba_ewram_write32(addr, value) 

    	Case &h03  ' IWRAM
        gba_iwram_write32(addr, value) 

    	Case &h04  ' IO
        gba_io_write32(addr, value) 

    	Case &h05  ' Palette
        gba_ppu_palette_write32(addr, value) 

    	Case &h06  ' VRAM
        gba_ppu_vram_write32(addr, value) 

    	Case &h07  ' OAM
        gba_ppu_oam_write32(addr, value) 

    	case &h0e, &h0f  ' Game Pak SRAM
        gba_cartridge_sram_write32(addr, value) 
    End Select

End Sub
