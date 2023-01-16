

static shared As BOOL gba_skipBoot 
static shared As BOOL gba_frame 

Declare Sub gba_cycle() 
Declare Sub gba_frameAdvance() 
Declare Sub gba_init(skipBoot As BOOL) 
Declare Sub gba_reset() 
Declare Sub gba_setBios(buffer As uint32_t Ptr) 
Declare Sub gba_setRom (buffer As uint32_t Ptr , size As size_t) 
Declare Sub gba_setSram(buffer As uint32_t Ptr , size As size_t) 

Declare Function gba_getSramSize() As size_t 



Sub gba_frameAdvance() 
    gba_frame = FALSE 

    while gba_frame=0 
        gba_cycle() 
    Wend
    
End Sub

Sub gba_cycle() 
    If gba_dma_cycle()=0 Then 
        gba_cpu_cycle() 
    End If
    gba_ppu_cycle() 
    gba_timer_cycle() 
End Sub

Function gba_getSramSize() As size_t 
    return 0 
End Function

Sub gba_reset() 
    gba_cpu_reset(gba_skipBoot) 
    gba_dma_reset() 
    gba_ewram_reset() 
    gba_io_reset() 
    gba_iwram_reset() 
    gba_ppu_reset() 
    gba_timer_reset() 
End Sub

Sub gba_init(skipBoot As BOOL) 
    gba_skipBoot = skipBoot 
    gba_reset() 
    gba_cpu_init() 
End Sub

Sub gba_setBios(buffer As uint32_t Ptr) 
    gba_bios_init(buffer) 
    gba_reset() 
End Sub

Sub gba_setRom(buffer As uint32_t Ptr , size As size_t) 
    gba_cartridge_init(buffer, size) 
    'gba_reset() 
End Sub

Sub gba_setSram(buffer As uint32_t Ptr , size As size_t) 
    UNUSED(buffer) 
    UNUSED(size) 
End Sub

Sub gba_setInterruptFlag(flag As uint16_t) 
    gba_io_getRegister(&h04000202)->value Or= flag 
End Sub

Sub gba_writeToIf (addr As uint32_t , flag As uint16_t) 
    UNUSED(addr) 
    gba_io_getRegister(&h04000202)->value And= NOT(flag) 
End Sub

Sub gba_onFrame() 
    gba_frame = TRUE 
End Sub
