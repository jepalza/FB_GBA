
' accesos a memoria

'#define ACCESS_N(bitsSize, array, offset) *((uint##bitsSize##_t *)(((size_t)(array) + (size_t)(offset))))
'#define ACCESS_8 (array, offset) ACCESS_N (8, array, offset)
'#define ACCESS_16(array, offset) ACCESS_N(16, array, offset)
'#define ACCESS_32(array, offset) ACCESS_N(32, array, offset)
function ACCESS_8(array As uint8_t ptr, offset As uint32_t) As uint8_t
	'Print "read 8 :";Hex(offset,8),Hex(array[offset],8):Sleep
	Return array[offset]
End Function
Function ACCESS_16(array As uint16_t Ptr, offset As uint32_t) As uint16_t
	'Print "read 16:";Hex(offset,8),Hex(array[offset],8)':Sleep
	Return array[offset Shr 1]
End Function
Function ACCESS_32(array As uint32_t ptr, offset As uint32_t) As uint32_t
	'Print "read 32:";Hex(offset,8),Hex(array[offset],8)':Sleep
	Return array[offset Shr 2]
End Function

sub  WACCESS_8(array As uint8_t Ptr, offset As uint32_t, value As uint8_t)
	'Print "write 8 :";Hex(offset,8),Hex(value,8):Sleep
	array[offset]=value
End Sub
Sub WACCESS_16(array As uint16_t Ptr, offset As uint32_t, value As uint16_t) 
	'Print "write 16 a:";Hex(offset,8),Hex(value,8)':Sleep
	array[offset Shr 1]=value
End Sub
Sub WACCESS_32(array As uint32_t ptr, offset As uint32_t, value As uint32_t)  
	'Print "write 32:";Hex(offset,8),Hex(value,8)':sleep
	array[offset Shr 2]=value
End Sub





' BIOS
static shared As any Ptr gba_bios_buffer  

Sub gba_bios_init(_buffer As uint32_t Ptr) 
    gba_bios_buffer = _buffer 
End Sub

Function gba_bios_read8(addr As uint32_t) As uint8_t 
    Return ACCESS_8(gba_bios_buffer, addr And &h00003fff) 
End Function

Function gba_bios_read16(addr As uint32_t) As uint16_t 
    Return ACCESS_16(gba_bios_buffer, addr And &h00003ffe) 
End Function

Function gba_bios_read32(addr As uINT32_T) As uint32_t 
	'If DEB>0 Then Print "BIOS:";Hex(addr,8),Hex(ACCESS_32(gba_bios_buffer, addr And &h00003ffc) ,8)  ':Sleep
    return ACCESS_32(gba_bios_buffer, addr And &h00003ffc) 
End Function






' EWRAM
static shared As Any Ptr gba_ewram_buffer_ptr=@gba_ewram_buffer(0)

Sub gba_ewram_reset() 
    memset(gba_ewram_buffer_ptr, 0, GBA_EWRAM_SIZE) 
End Sub

Function gba_ewram_read8(addr As uint32_t) As uint8_t 
    return gba_ewram_buffer(addr And &h0003ffff) 
End Function

Function gba_ewram_read16(addr As uint32_t) As uint16_t 
    return ACCESS_16(gba_ewram_buffer_ptr, addr And &h0003fffe)
End Function

Function gba_ewram_read32(addr As uint32_t) As uint32_t 
    Return ACCESS_32(gba_ewram_buffer_ptr, addr And &h0003fffc)
End Function

Sub gba_ewram_write8(addr As uint32_t , value As uint8_t) 
    gba_ewram_buffer(addr And &h0003ffff) = value 
End Sub

Sub gba_ewram_write16(addr As uint32_t , value As uint16_t) 
    WACCESS_16(gba_ewram_buffer_ptr, addr And &h0003fffe, value )
End Sub

Sub gba_ewram_write32(addr As uint32_t , value As uint32_t) 
    WACCESS_32(gba_ewram_buffer_ptr, addr And &h0003fffc, value )
End Sub









' IWRAM
static shared As Any ptr gba_iwram_buffer_ptr=@gba_iwram_buffer(0)

Sub gba_iwram_reset() 
    memset(gba_iwram_buffer_ptr, 0, GBA_IWRAM_SIZE) 
End Sub

Function gba_iwram_read8(addr As uint32_t) As uint8_t 
    return gba_iwram_buffer(addr And &h00007fff) 
End Function

Function gba_iwram_read16(addr As uint32_t) As uint16_t 
    Return ACCESS_16(gba_iwram_buffer_ptr, addr And &h00007ffe)
End Function

Function gba_iwram_read32(addr As uint32_t) As uint32_t 
    Return ACCESS_32(gba_iwram_buffer_ptr, addr And &h00007ffc)
End Function

Sub gba_iwram_write8(addr As uint32_t , value As uint8_t) 
    gba_iwram_buffer(addr And &h00007fff) = value 
End Sub

Sub gba_iwram_write16(addr As uint32_t , value As uint16_t) 
    WACCESS_16(gba_iwram_buffer_ptr, addr And &h00007ffe, value)
End Sub

Sub gba_iwram_write32(addr As uint32_t, value As uint32_t) 
    WACCESS_32(gba_iwram_buffer_ptr, addr And &h00007ffc, value)
End Sub




' SRAM
static shared As Any ptr gba_sram_buffer_ptr=@gba_sram_buffer(0)
Sub gba_sram_reset() 
    memset(gba_sram_buffer_ptr, &hFF, GBA_MAX_SRAM_FILE_SIZE) 
End Sub

Function gba_sram_read8(addr As uint32_t) As uint8_t 
	Dim As uint8_t ret=gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1))
	'Print "RD8 :";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(ret,2)
    return ret 
End Function

Function gba_sram_read16(addr As uint32_t) As uint16_t 
	Dim As uint16_t ret=gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1))
	ret=(ret Shl 8)+ret
		'Print "RD16:";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(ret,4)
    return ret 
End Function

Function gba_sram_read32(addr As uint32_t) As uint32_t 
	Dim As uint32_t ret=gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1))
	ret=(ret Shl 24)+(ret Shl 16)+(ret Shl 8)+ret
		'Print "RD32:";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(ret,8)
    return ret 
End Function

Sub gba_sram_write8(addr As uint32_t , value As uint8_t) 
		'Print "WR8 :";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(value,2)
    gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = value 
End Sub

Sub gba_sram_write16(addr As uint32_t , value As uint16_t) 
		'Print "WR16:";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(value,4)
		If (addr And 1)=0 then 
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = value And &hff
		Else
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = (value And &hff00) Shr 8
		EndIf
End Sub

Sub gba_sram_write32(addr As uint32_t, value As uint32_t) 
		'Print "WR32:";addr And (GBA_MAX_SRAM_FILE_SIZE-1),hex(value,8)
		If (addr And 3)=0 then 
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = value And &hff
		ElseIf (addr And 3)=1 then
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = (value And &hff00) Shr 8
		ElseIf (addr And 3)=2 Then
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = (value And &hff0000) Shr 16
		ElseIf (addr And 3)=3 Then
		gba_sram_buffer(addr And (GBA_MAX_SRAM_FILE_SIZE-1)) = (value And &hff000000) Shr 24
		EndIf
End Sub





' DMA
Enum gba_dma_channel_destinationAddressControl_t 
    GBA_DMA_CHANNEL_DAC_INCREMENT,
    GBA_DMA_CHANNEL_DAC_DECREMENT,
    GBA_DMA_CHANNEL_DAC_FIXED,
    GBA_DMA_CHANNEL_DAC_INCREMENT_RELOAD
End Enum 

Enum gba_dma_channel_sourceAddressControl_t 
    GBA_DMA_CHANNEL_SAC_INCREMENT,
    GBA_DMA_CHANNEL_SAC_DECREMENT,
    GBA_DMA_CHANNEL_SAC_FIXED,
    GBA_DMA_CHANNEL_SAC_PROHIBITED
End Enum 

Enum gba_dma_channel_startTiming_t 
    GBA_DMA_CHANNEL_STARTTIMING_IMMEDIATELY,
    GBA_DMA_CHANNEL_STARTTIMING_VBLANK,
    GBA_DMA_CHANNEL_STARTTIMING_HBLANK,
    GBA_DMA_CHANNEL_STARTTIMING_SPECIAL
End Enum 

Type gba_dma_channel_t 
    As Integer index 
    As uint32_t iobase 
    As uint32_t sourceAddress 
    As uint32_t destinationAddress 
    As uint32_t wordCount 
    As gba_dma_channel_destinationAddressControl_t destinationAddressControl 
    As gba_dma_channel_sourceAddressControl_t sourceAddressControl 
    As BOOL repeat 
    As BOOL bitWidth 
    As BOOL gamePakDRQ 
    As gba_dma_channel_startTiming_t startTiming 
    As BOOL irq 
    As BOOL enabled 
    As BOOL running 
End Type 

static shared As gba_dma_channel_t gba_dma_channels(4) 


Declare Function gba_dma_channel_cycle( channel As gba_dma_channel_t Ptr) As BOOL 

Declare sub gba_dma_channel_init  ( channel As gba_dma_channel_t Ptr , index As Integer)
Declare Sub gba_dma_channel_finish( channel As gba_dma_channel_t Ptr)
Declare sub gba_dma_channel_repeat( channel As gba_dma_channel_t Ptr)
Declare sub gba_dma_channel_reloadRegisters( channel As gba_dma_channel_t Ptr , repeat As BOOL)
Declare Sub gba_dma_channel_reloadSourceAddress( channel As gba_dma_channel_t Ptr)
Declare Sub gba_dma_channel_reloadDestinationAddress( channel As gba_dma_channel_t Ptr)
Declare sub gba_dma_channel_reloadWordCount( channel As gba_dma_channel_t Ptr)

'Declare Sub gba_dma_onVblank() 
'Declare Sub gba_dma_onHblank() 

Sub gba_dma_reset() 
    for i As integer = 0 To  3       
        gba_dma_channel_init(@gba_dma_channels(i), i) 
    Next
End Sub

Function gba_dma_cycle() As BOOL 
    for i As Integer = 0 To 3         
        If (gba_dma_channel_cycle(@gba_dma_channels(i))) Then Return TRUE 
    Next
    return FALSE 
End Function

Sub gba_dma_writeCallback_cntH( channel As gba_dma_channel_t Ptr , value As uint16_t)
    Dim As BOOL oldEnabled = channel->enabled 

    channel->destinationAddressControl = (value And &h0060) Shr 5 
    channel->sourceAddressControl      = (value And &h0180) Shr 7 
    channel->repeat      = leeBIT(value,9) 
    channel->bitWidth    = leeBIT(value,10)
    channel->gamePakDRQ  = leeBIT(value,11)
    channel->startTiming = leeBIT(value,12)
    channel->irq         = leeBIT(value,14)
    channel->enabled     = leeBIT(value,15)

    If (oldEnabled<>0) And (channel->enabled=0) Then 
        channel->running = FALSE 
    Else
        gba_dma_channel_reloadRegisters(channel, FALSE) 
        If (channel->startTiming = GBA_DMA_CHANNEL_STARTTIMING_IMMEDIATELY) Then     
            channel->running = TRUE   
        End If 
    End If
 
End Sub

Sub gba_dma_writeCallback_cntH0(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_dma_writeCallback_cntH(@gba_dma_channels(0), value) 
End Sub

Sub gba_dma_writeCallback_cntH1(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_dma_writeCallback_cntH(@gba_dma_channels(1), value) 
End Sub

Sub gba_dma_writeCallback_cntH2(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_dma_writeCallback_cntH(@gba_dma_channels(2), value) 
End Sub

Sub gba_dma_writeCallback_cntH3(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_dma_writeCallback_cntH(@gba_dma_channels(3), value) 
End Sub

Sub gba_dma_channel_init( channel As gba_dma_channel_t Ptr , index As Integer) 
    channel->index = index 
    channel->iobase = (GBA_REG_BASE+&hB0) + culng((index * 12))
End Sub

Function gba_dma_channel_cycle( channel As gba_dma_channel_t Ptr) As BOOL
    If (channel->running) Then 
      
        If (channel->bitWidth) Then 
          
            gba_bus_write32(channel->destinationAddress, gba_bus_read32(channel->sourceAddress)) 

            Select Case As Const   (channel->destinationAddressControl)  
            	case GBA_DMA_CHANNEL_DAC_INCREMENT  
            		channel->destinationAddress += 4
            	case GBA_DMA_CHANNEL_DAC_DECREMENT  
            		channel->destinationAddress -= 4
            	case GBA_DMA_CHANNEL_DAC_FIXED 
            		' no hace nada aqui
            	case GBA_DMA_CHANNEL_DAC_INCREMENT_RELOAD  
            		channel->destinationAddress += 4
            End Select


            Select Case As Const   (channel->sourceAddressControl)  
            	case GBA_DMA_CHANNEL_SAC_INCREMENT  
            		channel->sourceAddress += 4 
            	case GBA_DMA_CHANNEL_SAC_DECREMENT  
            		channel->sourceAddress -= 4
            	case GBA_DMA_CHANNEL_SAC_FIXED  
            		' no hace nada aqui
            	case GBA_DMA_CHANNEL_SAC_PROHIBITED  
            		channel->sourceAddress += 4
            End Select

          else
                 
            gba_bus_write16(channel->destinationAddress, gba_bus_read16(channel->sourceAddress)) 

            Select Case As Const   (channel->destinationAddressControl)  
            	case GBA_DMA_CHANNEL_DAC_INCREMENT  
            		channel->destinationAddress += 2
            	case GBA_DMA_CHANNEL_DAC_DECREMENT 
            		channel->destinationAddress -= 2
            	case GBA_DMA_CHANNEL_DAC_FIXED  
            		' no hace nada aqui 
            	case GBA_DMA_CHANNEL_DAC_INCREMENT_RELOAD  
            		channel->destinationAddress += 2 
            End Select


            Select Case As Const   (channel->sourceAddressControl)  
            	case GBA_DMA_CHANNEL_SAC_INCREMENT  
            		channel->sourceAddress += 2
            	case GBA_DMA_CHANNEL_SAC_DECREMENT  
            		channel->sourceAddress -= 2
            	case GBA_DMA_CHANNEL_SAC_FIXED  
            		' no hace nada aqui 
            	case GBA_DMA_CHANNEL_SAC_PROHIBITED  
            		channel->sourceAddress += 2
            End Select

        
        End If

        channel->wordCount-=1  

        If (channel->wordCount = 0) Then 
            gba_dma_channel_finish(channel)     
        End If
        return TRUE   
    
    End If
 
    Return FALSE  
End function

Sub gba_dma_channel_finish( channel As gba_dma_channel_t Ptr)
    channel->running = FALSE 

    If (channel->repeat) Then 
        gba_dma_channel_repeat(channel) 
        If (channel->startTiming = GBA_DMA_CHANNEL_STARTTIMING_IMMEDIATELY) Then 
            channel->running = TRUE 
        else    
        	channel->enabled = FALSE
        End If
        gba_io_getRegister(channel->iobase + 10)->value And= &h7fff 
    End If
 
    If (channel->irq) Then 
        gba_setInterruptFlag(1 Shl (channel->index + 8)) 
    End If
 
End Sub

Sub gba_dma_channel_repeat( channel As gba_dma_channel_t Ptr)
    gba_dma_channel_reloadRegisters(channel, TRUE) 
End Sub

Sub gba_dma_channel_reloadRegisters( channel As gba_dma_channel_t Ptr , repeat As BOOL)
    If (repeat) Then 
        If (channel->destinationAddressControl = GBA_DMA_CHANNEL_DAC_INCREMENT_RELOAD) Then 
            gba_dma_channel_reloadDestinationAddress(channel) 
        End If
        gba_dma_channel_reloadWordCount(channel) 
    else     
        gba_dma_channel_reloadSourceAddress(channel) 
        gba_dma_channel_reloadDestinationAddress(channel) 
        gba_dma_channel_reloadWordCount(channel) 
    End If
 
End Sub

Sub gba_dma_channel_reloadSourceAddress( channel As gba_dma_channel_t Ptr)
    channel->sourceAddress = culng(gba_io_getRegister(channel->iobase)->value) Or culng(gba_io_getRegister(channel->iobase + 2)->value Shl 16) 
End Sub

Sub gba_dma_channel_reloadDestinationAddress( channel As gba_dma_channel_t Ptr)
    channel->destinationAddress = culng(gba_io_getRegister(channel->iobase + 4)->value) Or culng(gba_io_getRegister(channel->iobase + 6)->value Shl 16) 
End Sub

Sub gba_dma_channel_reloadWordCount( channel As gba_dma_channel_t Ptr)
    channel->wordCount = gba_io_getRegister(channel->iobase + 8)->value 
    If (channel->wordCount = 0) Then 
        If (channel->iobase = (GBA_REG_BASE+&hD4)) Then 
            channel->wordCount = &h10000 
        else   
            channel->wordCount = &h4000 
        End If
    End If
End Sub

Sub gba_dma_onVblank() 
	Dim As Integer i
    for i = 0 To 3       
        If (gba_dma_channels(i).enabled<>0) And (gba_dma_channels(i).running=0)  _ 
        			And (gba_dma_channels(i).startTiming = GBA_DMA_CHANNEL_STARTTIMING_VBLANK) Then 
            gba_dma_channels(i).running = TRUE 
        End If
    Next

End Sub

Sub gba_dma_onHblank() 
	Dim As Integer i
    for i = 0 To 3        
        If (gba_dma_channels(i).enabled<>0) And (gba_dma_channels(i).running=0)  _ 
        			And (gba_dma_channels(i).startTiming = GBA_DMA_CHANNEL_STARTTIMING_HBLANK) Then 
            gba_dma_channels(i).running = TRUE 
        End If
    Next
End Sub

