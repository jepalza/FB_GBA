

static shared As uint32_t gba_ppu_frameBuffer(GBA_SCREEN_WIDTH * GBA_SCREEN_HEIGHT -1) 
static shared As uint32_t gba_ppu_currentRow 
static shared As uint32_t gba_ppu_currentColumn 
static shared As uint32_t gba_ppu_currentCycle 

static shared As UInteger gba_ppu_layers(3) 


Sub gba_ppu_reset() 
    memset(@gba_ppu_palette(0), 0, GBA_PALETTE_SIZE) 
    memset(@gba_ppu_vram(0), 0, GBA_VRAM_SIZE) 
    memset(@gba_ppu_oam(0), 0, GBA_OAM_SIZE) 
End Sub

Sub gba_ppu_cycle() 
	
    If ((gba_ppu_currentCycle And &h03) = &h03) Then 
        Dim As gba_io_register_t Ptr dispstat = gba_io_getRegister(GBA_REG_BASE+4) 
        If (gba_ppu_currentColumn = 308) Then  
            gba_ppu_currentColumn = 0 
            gba_ppu_currentRow+=1  

            dispstat->value And=  NOT(1 Shl 1) 

            If ((dispstat->value Shr 8) = gba_ppu_currentRow) Then 
                dispstat->value Or= (1 Shl 2) 
                If (dispstat->value And (1 Shl 5)) Then               
                  gba_setInterruptFlag(1 Shl 2) 
                End If
            Else   
                dispstat->value And=  NOT(1 Shl 2)
            End If
 
            If (gba_ppu_currentRow = 228) Then 
                gba_ppu_currentRow = 0 
                gba_ppu_currentCycle = 0 
                dispstat->value And=  NOT(1) 
            ElseIf (gba_ppu_currentRow = GBA_SCREEN_HEIGHT) Then    
                dispstat->value Or= 1
                If (dispstat->value And (1 Shl 3)) Then    
                    gba_setInterruptFlag(1) 
                End If
                gba_ppu_onVblank() 
            End If
 
            gba_io_getRegister(GBA_REG_BASE+6)->value = gba_ppu_currentRow 
            
        ElseIf (gba_ppu_currentColumn = GBA_SCREEN_WIDTH) Then 
                 
            dispstat->value Or= (1 Shl 1)

            If (dispstat->value And (1 Shl 4)) Then 
                gba_setInterruptFlag(1 Shl 1) 
                gba_dma_onHblank() 
            End If

            If (gba_ppu_currentRow < GBA_SCREEN_HEIGHT) Then 
                gba_ppu_drawLine() 
            End If

        End If
        gba_ppu_currentColumn+=1  
    End If
 
    gba_ppu_currentCycle+=1  
End Sub




' ========================== RAM PALETTE ===============================
Function gba_ppu_palette_read8(addr As uint32_t) As uint8_t 
    return gba_ppu_palette(addr And &h000003ff) 
End Function

Function gba_ppu_palette_read16(addr As uint32_t) As uint16_t 
    return ACCESS_16(@gba_ppu_palette(0), addr And &h000003fe) 
End Function

Function gba_ppu_palette_read32(addr As uint32_t) As uint32_t 
    return ACCESS_32(@gba_ppu_palette(0), addr And &h000003fc)
End function

Sub gba_ppu_palette_write8(addr As uint32_t , value As uint8_t) 
    gba_ppu_palette_write16(addr, cushort((value Shl 8) Or value)) 
End Sub

Sub gba_ppu_palette_write16(addr As uint32_t , value As uint16_t) 
    WACCESS_16(@gba_ppu_palette(0), addr And &h000003fe, value) 
End Sub

Sub gba_ppu_palette_write32(addr As uint32_t , value As uint32_t) 
    WACCESS_32(@gba_ppu_palette(0), addr And &h000003fc, value) 
End Sub




' ============================= RAM VIDEO ====================================
Function gba_ppu_vram_read8(addr As uint32_t) As uint8_t 
    If (addr And &h00010000) Then 
        addr And= &h00017fff 
    else 
        addr And= &h0001ffff 
    End If
    return gba_ppu_vram(addr) 
End Function

Function gba_ppu_vram_read16(addr As uint32_t) As uint16_t 
    If (addr And &h00010000) Then 
        addr And= &h00017ffe 
    else   
        addr And= &h0001fffe 
    End If
    return ACCESS_16(@gba_ppu_vram(0), addr) 
End Function

Function gba_ppu_vram_read32(addr As uint32_t) As uint32_t 
    If (addr And &h00010000) Then 
        addr And= &h00017ffc 
    else  
        addr And= &h0001fffc 
    End If
    return ACCESS_32(@gba_ppu_vram(0), addr) 
End Function

Sub gba_ppu_vram_write8(addr As uint32_t , value As uint8_t) 
    gba_ppu_vram_write16(addr, cushort((value Shl 8) Or value)) 
End Sub

Sub gba_ppu_vram_write16(addr As uint32_t , value As uint16_t) 
    If (addr And &h00010000) Then 
        addr And= &h00017ffe 
    Else 
        addr And= &h0001fffe 
    End If
    WACCESS_16(@gba_ppu_vram(0), addr, value)
End Sub

Sub gba_ppu_vram_write32(addr As uint32_t , value As uint32_t) 
    If (addr And &h00010000) Then 
        addr And= &h00017ffc 
    else 
        addr And= &h0001fffc 
    End If
    WACCESS_32(@gba_ppu_vram(0), addr, value) 
End Sub




' =========================== RAM GRAFICOS (TILES) =================================
Function gba_ppu_oam_read8(addr As uint32_t) As uint8_t 
    return gba_ppu_oam(addr And &h000003ff) 
End Function

Function gba_ppu_oam_read16(addr As uint32_t) As uint16_t 
    return ACCESS_16(@gba_ppu_oam(0), addr And &h000003fe) 
End Function

Function gba_ppu_oam_read32(addr As uint32_t) As uint32_t 
    return ACCESS_32(@gba_ppu_oam(0), addr And &h000003fc) 
End Function

Sub gba_ppu_oam_write8(addr As uint32_t , value As uint8_t) 
    gba_ppu_oam_write16(addr, cushort((value Shl 8) Or value)) 
End Sub

Sub gba_ppu_oam_write16(addr As uint32_t , value As uint16_t) 
    WACCESS_16(@gba_ppu_oam(0), addr And &h000003fe, value) 
End Sub

Sub gba_ppu_oam_write32(addr As uint32_t , value As uint32_t) 
    WACCESS_32(@gba_ppu_oam(0), addr And &h000003fc, value)
End Sub




' ===============================================================================
Function gba_ppu_colorToRgb(color_ As uint16_t) As uint32_t
    Dim As UInteger blue  = (color_ And &h7c00) Shr 10 
    Dim As UInteger green = (color_ And &h03e0) Shr 5 
    Dim As UInteger red   =  color_ And &h001f 

    return &hff000000 Or (red Shl 3) Or (green Shl 11) Or (blue Shl 19) 
End Function

Function gba_ppu_getPaletteColor(index As uint8_t) As uint16_t
    return gba_ppu_palette_read16(&h05000000 Or (index Shl 1)) 
End Function

Sub gba_ppu_sortLayers() 
    gba_ppu_layers(0) = 3 
    gba_ppu_layers(1) = 2 
    gba_ppu_layers(2) = 1 
    gba_ppu_layers(3) = 0 

    Dim As Integer priorities(3) 
    Dim As Integer i,j

    for i = 0 To 3        
        priorities(i) = gba_io_getRegister((GBA_REG_BASE+8) + (i Shl 1))->value And &h0003 
    Next

    for i = 0 To 2        
        Dim As Integer layerPriority = priorities(i) 
        Dim As Integer maxPriority = layerPriority 
        Dim As Integer maxPriorityIndex = i 

        for j = i + 1 To 3        
            Dim As Integer currentLayerPriority = priorities(j) 
            If (currentLayerPriority > maxPriority) Then 
                maxPriority = currentLayerPriority 
                maxPriorityIndex = j 
            End If
        Next

        Dim As UInteger exchange = gba_ppu_layers(i) 
        gba_ppu_layers(i) = gba_ppu_layers(maxPriorityIndex) 
        gba_ppu_layers(maxPriorityIndex) = exchange 
    Next

End Sub

Sub gba_ppu_drawLayer(layer As Integer) 
    Dim As gba_io_register_t Ptr bgcnt = gba_io_getRegister((GBA_REG_BASE+8) + (layer Shl 1)) 

    Dim As UInteger hofs = gba_io_getRegister((GBA_REG_BASE+10) + (layer Shl 2))->value 
    Dim As UInteger vofs = gba_io_getRegister((GBA_REG_BASE+12) + (layer Shl 2))->value 
    Dim As uint32_t mapBase  = (bgcnt->value And &h1f00) Shl 3 
    Dim As uint32_t tileBase = (bgcnt->value And &h000c) Shl 12 
    Dim As UInteger yLayer   = gba_ppu_currentRow + vofs 

    Dim As uint32_t mapOffsetY = &h00000000 

    If (bgcnt->value And (1 Shl 15)) Then 
        yLayer And= &h000001ff 
        If (((bgcnt->value And &hc000) = &hc000) And (yLayer >= &h100)) Then     
          mapOffsetY = &h00001000  
        End If  
    Else       
        yLayer And= &h000000ff
    End If
 
    Dim As UInteger yChunk = yLayer And &h000000ff 
    Dim As UInteger yMap   = yChunk Shr 3 
    Dim As UInteger yTile  = yChunk And 7 
    
    Dim As UInteger x

    for x = 0 To GBA_SCREEN_WIDTH -1        
        Dim As UInteger xLayer = x + hofs 
        Dim As uint32_t mapOffsetX = &h00000000 

        If (bgcnt->value And (1 Shl 14)) Then 
            xLayer And= &h000001ff 
            If ((bgcnt->value And (1 Shl 14)) And (xLayer >= &h100)) Then  
                mapOffsetX = &h00000800 
            End If
        Else
            xLayer And= &h000000ff
        End If
 

        Dim As UInteger xChunk = xLayer And &h000000ff 
        Dim As UInteger xMap   = xChunk Shr  3 
        Dim As UInteger xTile  = xChunk And &h07 
        Dim As uint32_t mapOffset  = mapBase + mapOffsetX + mapOffsetY 
        Dim As uint32_t mapAddress = mapOffset + (yMap Shl 6) + (xMap Shl 1) 
        Dim As uint16_t mapValue   = gba_ppu_vram(mapAddress) Or (gba_ppu_vram(mapAddress + 1) Shl 8) 
        Dim As uint16_t tileNumber = mapValue And &h03ff 
        Dim As BOOL flipHorizontal = leeBIT(mapvalue,10) 
        Dim As BOOL flipVertical   = leeBIT(mapvalue,11) 

        Dim As UInteger yTileReal = IIf(flipVertical   , 7 - yTile , yTile)
        Dim As UInteger xTileReal = IIf(flipHorizontal , 7 - xTile , xTile) 

        Dim As uint32_t colorAddress 

        If (bgcnt->value And (1 Shl 7)) Then 
	         Dim As uint32_t tileDataAddress = (tileNumber Shl 6) Or (yTileReal Shl 3) Or xTileReal 
	         Dim As uint8_t tileValue = gba_ppu_vram(tileBase + tileDataAddress) 
	         colorAddress = tileValue Shl 1 
        Else
            Dim As uint32_t tileDataAddress = (tileNumber Shl 5) Or (yTileReal Shl 2) Or (xTileReal Shr 1) 
            Dim As uint8_t  tileValue = gba_ppu_vram(tileBase + tileDataAddress) 

            if ((xTileReal And 1) Xor flipHorizontal) Then ' antes=0 , o sea, NOT
                tileValue Shr= 4 
            Else     
                tileValue And= &h0f 
            End If

            Dim As UInteger paletteNumber = mapValue Shr 12 
            colorAddress = (paletteNumber Shl 5) Or (tileValue Shl 1) 
        End If
 
        Dim As uint16_t pixelColor = gba_ppu_palette(colorAddress) Or (gba_ppu_palette(colorAddress + 1) Shl 8) 
        gba_ppu_frameBuffer(gba_ppu_currentRow * GBA_SCREEN_WIDTH + x) = gba_ppu_colorToRgb(pixelColor) 
    
   Next

End Sub


' BG Mode 0 - 240x160 pixels, Text mode
Sub gba_ppu_drawMode0() 
    gba_ppu_sortLayers() 

    Dim As gba_io_register_t Ptr dispcnt = gba_io_getRegister(GBA_REG_BASE) 

    for i As integer= 0 To 3        
        If (dispcnt->value And (1 Shl (8 + i))) Then 
            gba_ppu_drawLayer(i) 
        End If
    Next

End Sub

' BG Mode 1 - 240x160 pixels, Text and RS mode mixed
Sub gba_ppu_drawMode1() 
    gba_ppu_drawMode0() 
End Sub


' BG Mode 2 - 240x160 pixels, RS mode
Sub gba_ppu_drawMode2() 
    gba_ppu_drawMode0() 
End Sub

' BG Mode 3 - 240x160 pixels, 32768 colors
Sub gba_ppu_drawMode3() 
    for x As integer = 0 To GBA_SCREEN_WIDTH-1         
        gba_ppu_frameBuffer(gba_ppu_currentRow * GBA_SCREEN_WIDTH + x) = _
        		gba_ppu_colorToRgb(gba_ppu_vram_read16(&h06000000 Or ((gba_ppu_currentRow * GBA_SCREEN_WIDTH + x) Shl 1))) 
    Next

End Sub

' BG Mode 4 - 240x160 pixels, 256 colors (out of 32768 colors)
Sub gba_ppu_drawMode4() 
    Dim As gba_io_register_t Ptr dispcnt = gba_io_getRegister(GBA_REG_BASE) 

    Dim As uint32_t offset = IIf((dispcnt->value And (1 Shl 4)) , &h0000A000 , &h00000000 )

    for x As integer = 0 To GBA_SCREEN_WIDTH-1 
        gba_ppu_frameBuffer(gba_ppu_currentRow * GBA_SCREEN_WIDTH + x) = _
        		gba_ppu_colorToRgb(gba_ppu_getPaletteColor(gba_ppu_vram(gba_ppu_currentRow * GBA_SCREEN_WIDTH + x + offset))) 
    Next

End Sub

' BG Mode 5 - 160x128 pixels, 32768 colors
Sub gba_ppu_drawMode5() 
    Dim As gba_io_register_t Ptr dispcnt = gba_io_getRegister(GBA_REG_BASE) 

    Dim As uint32_t offset = IIf((dispcnt->value And (1 Shl 4)) , &h00005000 , &h00000000 )

    Dim As uInteger currentRow = gba_ppu_currentRow - 16 

    If (currentRow < GBA_MODE5_HEIGHT) Then 
        for x As integer = 0 To GBA_MODE5_WIDTH-1        
            gba_ppu_frameBuffer(currentRow * GBA_SCREEN_WIDTH + x + 40) = _
            	gba_ppu_colorToRgb(gba_ppu_vram_read16(&h06000000 Or ((currentRow * GBA_MODE5_WIDTH + x + offset) Shl 1))) 
        Next
    End If
 
End Sub

Sub gba_ppu_drawLine() 
    Dim As gba_io_register_t Ptr dispcnt = gba_io_getRegister(GBA_REG_BASE) 

    Select Case As Const   (dispcnt->value And &h0007)  
    	case 0  
    		gba_ppu_drawMode0() 
    	case 1 
    		gba_ppu_drawMode1()
    	case 2  
    		gba_ppu_drawMode2()
    	case 3  
    		gba_ppu_drawMode3() 
    	case 4  
    		gba_ppu_drawMode4()
    	case 5  
    		gba_ppu_drawMode5()
    	Case Else ' modos 6 y 7 dan error, no existen
    		Print "Error modo grafico 6 o 7":Sleep:End
    End Select

End Sub

Sub gba_ppu_onVblank() 
    gba_dma_onVblank() 
    frontend_frame(@gba_ppu_frameBuffer(0)) 
    gba_onFrame() 
End Sub

Sub gba_ppu_onHblank() 
    gba_dma_onHblank() 
End Sub
