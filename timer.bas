
Type gba_timer_channel_s 
    As gba_timer_channel_s Ptr nextChannel 
    As uint16_t irqFlag 
    As Integer index 
    As uint16_t reloadValue 
    As uint32_t prescaler 
    As BOOL countUp 
    As BOOL irq 
    As BOOL operate 
    As uint16_t counter 
End Type 
Type gba_timer_channel_t As gba_timer_channel_s


static shared As gba_timer_channel_t gba_timer_channels(3) 
static shared As uint32_t gba_timer_cycleCounter 


Declare Sub gba_timer_channel_init( channel As gba_timer_channel_t Ptr ,  nextChannel As gba_timer_channel_t Ptr , index As Integer)
Declare sub gba_timer_channel_cycle( channel As gba_timer_channel_t Ptr , cycleCounter As uint32_t )
Declare Sub gba_timer_channel_increment( channel As gba_timer_channel_t Ptr)
Declare Sub gba_timer_channel_lastOverflowed( channel As gba_timer_channel_t Ptr)


Sub gba_timer_reset() 
    gba_timer_channel_init(@gba_timer_channels(3), 0, 3) 
    gba_timer_channel_init(@gba_timer_channels(2), @gba_timer_channels(3), 2) 
    gba_timer_channel_init(@gba_timer_channels(1), @gba_timer_channels(2), 1) 
    gba_timer_channel_init(@gba_timer_channels(0), @gba_timer_channels(1), 0) 
End Sub

Sub gba_timer_cycle() 
    For i as integer = 0 To 3         
        gba_timer_channel_cycle(@gba_timer_channels(i), gba_timer_cycleCounter) 
    Next

    gba_timer_cycleCounter+=1  
End Sub

Sub gba_timer_channel_init( channel As gba_timer_channel_t Ptr ,  nextChannel As gba_timer_channel_t Ptr , index As Integer)
    channel->nextChannel = nextChannel 
    channel->irqFlag = 1 Shl (index + 3) 
    channel->index = index 
End Sub

Sub gba_timer_channel_cycle( channel As gba_timer_channel_t Ptr , cycleCounter As uint32_t)
    If (channel->operate<>0) And (channel->countUp=0) Then 
        If (cycleCounter And channel->prescaler) = 0 Then   
            gba_timer_channel_increment(channel) 
        End If
    End If
 
End Sub

Sub gba_timer_channel_increment( channel As gba_timer_channel_t Ptr)
    channel->counter+=1  

    If (channel->counter = 0) Then 
        If (channel->irq) Then 
            gba_setInterruptFlag(channel->irqFlag) 
        End If
 
        If (channel->nextChannel) Then 
            gba_timer_channel_lastOverflowed(channel->nextChannel) 
        End If

        channel->counter = channel->reloadValue 
    End If
 
End Sub

Sub gba_timer_channel_lastOverflowed( channel As gba_timer_channel_t Ptr)
    If (channel->countUp) Then 
        gba_timer_channel_increment(channel) 
    End If
End Sub

Sub gba_timer_writeCallback_channel_reload(index As Integer , value As uint16_t)
    gba_timer_channels(index).reloadValue = value 
End Sub

Sub gba_timer_writeCallback_channel_control(index As Integer , value As uint16_t)
    Select Case As Const   (value And &h0003)  
    	case 0  
    		gba_timer_channels(index).prescaler = 0
    	case 1  
    		gba_timer_channels(index).prescaler = 63 
    	case 2  
    		gba_timer_channels(index).prescaler = 255
    	case 3  
    		gba_timer_channels(index).prescaler = 1023 
    End Select

    If (index = 0) Then 
        gba_timer_channels(index).countUp = FALSE ' esto puede no estar bien
        gba_io_getRegister(&h04000102)->value And= &hfffb 
    else  
        gba_timer_channels(index).countUp = leeBIT(value,2) ' esto puede no estar bien
    End If

    gba_timer_channels(index).irq = leeBIT(value,6) 
    gba_timer_channels(index).operate = leeBIT(value,7) 
End Sub

Sub gba_timer_writeCallback_channel0_reload(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_reload(0, value) 
End Sub

Sub gba_timer_writeCallback_channel0_control(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_control(0, value) 
End Sub

Sub gba_timer_writeCallback_channel1_reload(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_reload(1, value) 
End Sub

Sub gba_timer_writeCallback_channel1_control(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_control(1, value) 
End Sub

Sub gba_timer_writeCallback_channel2_reload(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_reload(2, value) 
End Sub

Sub gba_timer_writeCallback_channel2_control(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_control(2, value) 
End Sub

Sub gba_timer_writeCallback_channel3_reload(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_reload(3, value) 
End Sub

Sub gba_timer_writeCallback_channel3_control(addr As uint32_t , value As uint16_t) 
    UNUSED(addr) 
    gba_timer_writeCallback_channel_control(3, value) 
End Sub
