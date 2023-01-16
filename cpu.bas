
Enum gba_cpu_mode_t 
    GBA_CPU_MODE_USR_OLD = &h00,
    GBA_CPU_MODE_FIQ_OLD = &h01,
    GBA_CPU_MODE_IRQ_OLD = &h02,
    GBA_CPU_MODE_SVC_OLD = &h03,
    
    GBA_CPU_MODE_USR 	 = &h10,
    GBA_CPU_MODE_FIQ 	 = &h11,
    GBA_CPU_MODE_IRQ 	 = &h12,
    GBA_CPU_MODE_SVC 	 = &h13,
    GBA_CPU_MODE_ABT 	 = &h17,
    GBA_CPU_MODE_UND 	 = &h1b,
    GBA_CPU_MODE_SYS 	 = &h1f
End Enum 

Enum gba_cpu_pipelineState_t 
    GBA_CPU_PIPELINESTATE_FLUSH,
    GBA_CPU_PIPELINESTATE_FETCH,
    GBA_CPU_PIPELINESTATE_DECODE,
    GBA_CPU_PIPELINESTATE_EXECUTE
End Enum 

Enum gba_cpu_condition_t 
    GBA_CPU_CONDITION_EQ,
    GBA_CPU_CONDITION_NE,
    GBA_CPU_CONDITION_CS,
    GBA_CPU_CONDITION_CC,
    GBA_CPU_CONDITION_MI,
    GBA_CPU_CONDITION_PL,
    GBA_CPU_CONDITION_VS,
    GBA_CPU_CONDITION_VC,
    GBA_CPU_CONDITION_HI,
    GBA_CPU_CONDITION_LS,
    GBA_CPU_CONDITION_GE,
    GBA_CPU_CONDITION_LT,
    GBA_CPU_CONDITION_GT,
    GBA_CPU_CONDITION_LE,
    GBA_CPU_CONDITION_AL,
    GBA_CPU_CONDITION_NV
End Enum 


Type gba_cpu_opcodeHandlerArm_t As Sub(opcode As uint32_t)
static shared As gba_cpu_opcodeHandlerArm_t gba_cpu_decodedOpcodeArmHandler 
static shared As gba_cpu_opcodeHandlerArm_t gba_cpu_decodeTable_arm(4095)


Type gba_cpu_opcodeHandlerThumb_t As Sub(opcode As uint16_t)
static shared As gba_cpu_opcodeHandlerThumb_t gba_cpu_decodedOpcodeThumbHandler 
static shared As gba_cpu_opcodeHandlerThumb_t gba_cpu_decodeTable_thumb(1023) 




static shared As gba_cpu_pipelineState_t gba_cpu_pipelineState 

static shared As uint32_t gba_cpu_r(15) 
static shared As uint32_t gba_cpu_r_usr(6) 
static shared As uint32_t gba_cpu_r_irq(1) 
static shared As uint32_t gba_cpu_r_fiq(6) 
static shared As uint32_t gba_cpu_r_svc(1) 
static shared As uint32_t gba_cpu_r_abt(1) 
static shared As uint32_t gba_cpu_r_und(1) 
static shared As uint32_t gba_cpu_spsr_irq 
static shared As uint32_t gba_cpu_spsr_fiq 
static shared As uint32_t gba_cpu_spsr_svc 
static shared As uint32_t gba_cpu_spsr_abt 
static shared As uint32_t gba_cpu_spsr_und 

static shared As gba_cpu_mode_t gba_cpu_mode 

static shared As uint32_t gba_cpu_fetchedOpcodeArm 
static shared As uint16_t gba_cpu_fetchedOpcodeThumb 
static shared As uint32_t gba_cpu_decodedOpcodeArmValue 
static shared As uint16_t gba_cpu_decodedOpcodeThumbValue 

static shared As uint32_t gba_cpu_shifterResult 

static shared As BOOL gba_cpu_flagN 
static shared As BOOL gba_cpu_flagZ 
static shared As BOOL gba_cpu_flagC 
static shared As BOOL gba_cpu_flagV 
static shared As BOOL gba_cpu_flagI 
static shared As BOOL gba_cpu_flagF 
static shared As BOOL gba_cpu_flagT 
static shared As BOOL gba_cpu_shifterCarry 

Declare Function gba_cpu_getCpsr() As uint32_t 
Declare Sub gba_cpu_setCpsr(value As uint32_t) 
Declare Function gba_cpu_getSpsr() As uint32_t 
Declare Sub gba_cpu_setSpsr(value As uint32_t) 
Declare Sub gba_cpu_changeMode(newMode As gba_cpu_mode_t) 
Declare Function gba_cpu_checkCondition(condition As gba_cpu_condition_t) As BOOL 
Declare Sub gba_cpu_performJump(addr As uint32_t) 
Declare Sub gba_cpu_raiseIrq() 
Declare Sub gba_cpu_raiseSwi() 
Declare Sub gba_cpu_raiseUnd() 
Declare Sub gba_cpu_execute() 
Declare Sub gba_cpu_decode() 
Declare Sub gba_cpu_fetch(addr As uint32_t) 
Declare Sub gba_cpu_initDecodeThumb() 
Declare Sub gba_cpu_initDecodeArm() 
Declare Sub gba_cpu_arm_shift(opcode As uint32_t) 
Declare Function gba_cpu_util_ror32(value As uint32_t , bits As Integer) As uint32_t 
Declare Sub gba_cpu_writeRegister(r As Integer , value As uint32_t) 
Declare Sub gba_cpu_setFlags_logical(result As uint32_t) 
Declare Sub gba_cpu_setFlags_arithmetical(result As uint32_t) 

Declare Function gba_cpu_getCarry_sbc(left_ As uint32_t , right_ As uint32_t , carry As BOOL) As BOOL 
Declare Function gba_cpu_getCarry_sub(left_ As uint32_t , right_ As uint32_t) As BOOL 
Declare Function gba_cpu_getOverflow_sub(left_ As uint32_t , right_ As uint32_t , result As uint32_t) As BOOL 
Declare Function gba_cpu_getOverflow_add(left_ As uint32_t , right_ As uint32_t , result As uint32_t) As BOOL 
'
Declare Function gba_cpu_util_hammingWeight16(value As uint16_t) As UInteger 
Declare Function gba_cpu_util_hammingWeight8 (value As uint8_t)  As uInteger 
'
Declare Sub gba_cpu_arm_psrTransfer_msr(opcode As uint32_t , spsr As BOOL , operand As uint32_t) 
'
Declare Sub gba_cpu_arm_halfwordSignedDataTransfer(opcode As uint32_t) 
Declare Sub gba_cpu_arm_blockDataTransfer(opcode As uint32_t) 
Declare Sub gba_cpu_arm_swp(opcode As uint32_t) 
Declare Sub gba_cpu_arm_mul(opcode As uint32_t) 
Declare Sub gba_cpu_arm_mull(opcode As uint32_t) 
Declare Sub gba_cpu_arm_bx(opcode As uint32_t) 
Declare Sub gba_cpu_arm_swi(opcode As uint32_t) 
Declare Sub gba_cpu_arm_singleDataTransfer(opcode As uint32_t) 
Declare Sub gba_cpu_arm_psrTransfer(opcode As uint32_t) 
Declare Sub gba_cpu_arm_and(opcode As uint32_t) 
Declare Sub gba_cpu_arm_eor(opcode As uint32_t) 
Declare Sub gba_cpu_arm_sub(opcode As uint32_t) 
Declare Sub gba_cpu_arm_rsb(opcode As uint32_t) 
Declare Sub gba_cpu_arm_add(opcode As uint32_t) 
Declare Sub gba_cpu_arm_adc(opcode As uint32_t) 
Declare Sub gba_cpu_arm_sbc(opcode As uint32_t) 
Declare Sub gba_cpu_arm_rsc(opcode As uint32_t) 
Declare Sub gba_cpu_arm_tst(opcode As uint32_t) 
Declare Sub gba_cpu_arm_teq(opcode As uint32_t) 
Declare Sub gba_cpu_arm_cmp(opcode As uint32_t) 
Declare Sub gba_cpu_arm_cmn(opcode As uint32_t) 
Declare Sub gba_cpu_arm_orr(opcode As uint32_t) 
Declare Sub gba_cpu_arm_mov(opcode As uint32_t) 
Declare Sub gba_cpu_arm_bic(opcode As uint32_t) 
Declare Sub gba_cpu_arm_mvn(opcode As uint32_t) 
Declare Sub gba_cpu_arm_b(opcode As uint32_t) 
'
Declare Sub gba_cpu_thumb_sub(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_add(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_lsl(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_lsr(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_asr(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_mov(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_cmp(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_add2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_sub2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldrStrh(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldrStr(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldr(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_bx(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_add3(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_cmp3(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_mov2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_and(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_eor(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_lsl2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_lsr2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_asr2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_adc(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_sbc(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ror(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_tst(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_neg(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_cmp2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_cmn(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_orr(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_mul(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_bic(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_mvn(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldrStr2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldrStr3(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldrStrh2(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_add5(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_pushPop(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_add4(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_swi(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_b(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_ldmStm(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_bl(opcode As uint16_t) 
Declare Sub gba_cpu_thumb_b2(opcode As uint16_t) 

Sub gba_cpu_init() 
    gba_cpu_initDecodeArm() 
    gba_cpu_initDecodeThumb() 
End Sub

Sub gba_cpu_reset(skipBoot As BOOL) 
    gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_FETCH 

    For i as Integer=0 To 1         
        gba_cpu_r_irq(i) = 0 
        gba_cpu_r_svc(i) = 0 
        gba_cpu_r_abt(i) = 0 
        gba_cpu_r_und(i) = 0 
    Next

    For i as Integer=0 To 6         
        gba_cpu_r_fiq(i) = 0 
        gba_cpu_r_usr(i) = 0 
    Next

    For i as Integer=0 To 15   
        gba_cpu_r(i) = 0 
    Next

    gba_cpu_flagN = 0 
    gba_cpu_flagZ = 0 
    gba_cpu_flagC = 0 
    gba_cpu_flagV = 0 
    gba_cpu_flagI = 0 
    gba_cpu_flagF = 0 
    gba_cpu_flagT = 0 

    If (skipBoot) Then 
        gba_cpu_r(13)    = &h03007f00 
        gba_cpu_r_irq(0) = &h03007fa0 
        gba_cpu_r_svc(0) = &h03007fe0 
        gba_cpu_r(15)    = &h08000000 
        gba_cpu_mode     = GBA_CPU_MODE_SYS 
    Else 
        gba_cpu_mode  = GBA_CPU_MODE_SVC 
        gba_cpu_flagI = 1 
        gba_cpu_flagF = 1 
    End If
 
    gba_cpu_spsr_abt = 0 
    gba_cpu_spsr_fiq = 0 
    gba_cpu_spsr_irq = 0 
    gba_cpu_spsr_svc = 0 
    gba_cpu_spsr_und = 0 
End Sub




' ---------------------------------------------------------
Sub gba_cpu_deb()

   'if gba_cpu_flagN<>1 and gba_cpu_flagN<>0 then print "gba_cpu_flagN ";gba_cpu_flagN 
   'if gba_cpu_flagZ<>1 and gba_cpu_flagZ<>0 then print "gba_cpu_flagZ ";gba_cpu_flagZ 
   'if gba_cpu_flagV<>1 and gba_cpu_flagV<>0 then print "gba_cpu_flagV ";gba_cpu_flagV 
   'if gba_cpu_flagC<>1 and gba_cpu_flagC<>0 then print "gba_cpu_flagC ";gba_cpu_flagC 
   'if gba_cpu_flagI<>1 and gba_cpu_flagI<>0 then print "gba_cpu_flagI ";gba_cpu_flagI 
   'if gba_cpu_flagF<>1 and gba_cpu_flagF<>0 then print "gba_cpu_flagF ";gba_cpu_flagF 
   'if gba_cpu_flagT<>1 and gba_cpu_flagT<>0 then print "gba_cpu_flagTgba_cpu_shifterCarry ";gba_cpu_flagT 
   'if gba_cpu_shifterCarry<>1 and gba_cpu_shifterCarry<>0 then print gba_cpu_shifterCarry

	'gba_cpu_flagN =	IIf( gba_cpu_flagN ,1 ,0)
	'gba_cpu_flagZ =	IIf( gba_cpu_flagZ ,1 ,0) 
	'gba_cpu_flagC =	IIf( gba_cpu_flagC ,1 ,0) 
	'gba_cpu_flagV =	IIf( gba_cpu_flagV ,1 ,0) 
	'gba_cpu_flagI =	IIf( gba_cpu_flagI ,1 ,0) 
	'gba_cpu_flagF =	IIf( gba_cpu_flagF ,1 ,0) 
	'gba_cpu_flagT =	IIf( gba_cpu_flagT ,1 ,0) 
	'gba_cpu_shifterCarry = IIf( gba_cpu_shifterCarry ,1 ,0)
	
	If DEB>1 Then
	  Print "----------------------------------------------------------------------------------------------------------"
	  Print "DIR:";hex( IIf(gba_cpu_flagT , (gba_cpu_r(15) - 4) , (gba_cpu_r(15) - 8)),8);" ";
	  Print "   CPSR:";hex( gba_cpu_getCpsr(), 8);
     Print "   SPSR:";hex( gba_cpu_getSpsr(), 8)
     
	  Print "NZVC - IFTS"
     print gba_cpu_flagN ;
     print gba_cpu_flagZ ;
     print gba_cpu_flagV ;
     print gba_cpu_flagC ;" - ";
     print gba_cpu_flagI ;
     print gba_cpu_flagF ;
     print gba_cpu_flagT ;
     print gba_cpu_shifterCarry
     
     Print " r0:";hex( gba_cpu_r(0), 8);
     Print " r1:";hex( gba_cpu_r(1), 8);
     Print "  r2:";hex( gba_cpu_r(2), 8);
     Print "  r3:";hex( gba_cpu_r(3), 8);
     Print "  r4:";hex( gba_cpu_r(4), 8);
     Print "  r5:";hex( gba_cpu_r(5), 8);
     Print "  r6:";hex( gba_cpu_r(6), 8);
     Print "  r7:";hex( gba_cpu_r(7), 8)
     Print " r8:";hex( gba_cpu_r(9), 8);
     Print " r9:";hex( gba_cpu_r(9), 8);
     Print " r10:";hex( gba_cpu_r(10), 8);
     Print " r11:";hex( gba_cpu_r(11), 8);
     Print " r12:";hex( gba_cpu_r(12), 8);
     Print " r13:";hex( gba_cpu_r(13), 8);
     Print " r14:";hex( gba_cpu_r(14), 8);
     Print " r15:";hex( gba_cpu_r(15), 8)
     Print
     'If DEB=3 Then Sleep
  	EndIf

End Sub
Sub gba_cpu_execute() 
	
    If (gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_EXECUTE) Then 
        ' Check for interrupts
        If ((gba_io_read16(&h04000208) And &h0001)<>0) _
           And ((gba_io_read16(&h04000200) And gba_io_read16(&h04000202) And &h3fff)<>0) _
           And (gba_cpu_flagI=0) Then 
            	gba_cpu_raiseIrq() 
        Else
				' depuracion
				If DEB Then gba_cpu_deb()
        	
            If (gba_cpu_flagT) Then 
                If (gba_cpu_decodedOpcodeThumbHandler) Then 
                   gba_cpu_decodedOpcodeThumbHandler(gba_cpu_decodedOpcodeThumbValue) 
                Else
                   gba_cpu_raiseUnd() 
                End If
            Else
                If (gba_cpu_decodedOpcodeArmHandler) Then 
                   If (gba_cpu_checkCondition(gba_cpu_decodedOpcodeArmValue Shr 28)) Then 
                      gba_cpu_decodedOpcodeArmHandler(gba_cpu_decodedOpcodeArmValue) 
                   End If
                Else 
                   gba_cpu_raiseUnd()
                End If
            End If
        End If
    End If 

End Sub

Sub gba_cpu_decode() 

    If (gba_cpu_pipelineState >= GBA_CPU_PIPELINESTATE_DECODE) Then 
        If (gba_cpu_flagT) Then 
            gba_cpu_decodedOpcodeThumbValue   = gba_cpu_fetchedOpcodeThumb 
            gba_cpu_decodedOpcodeThumbHandler = gba_cpu_decodeTable_thumb(gba_cpu_fetchedOpcodeThumb Shr 6) 
        else  
            gba_cpu_decodedOpcodeArmValue   = gba_cpu_fetchedOpcodeArm 
            gba_cpu_decodedOpcodeArmHandler = gba_cpu_decodeTable_arm( ((gba_cpu_fetchedOpcodeArm Shr 16) And &h0ff0) _ 
            											 Or ((gba_cpu_fetchedOpcodeArm Shr 4) And &h0f) ) 
        End If
    End If
End Sub






' ---------------------------------------------------------

Sub gba_cpu_cycle() 
    Dim As uint32_t fetchAddress = gba_cpu_r(15) 

    gba_cpu_execute() 
    gba_cpu_decode() 

    gba_cpu_fetch(fetchAddress) 

    Select Case As Const   (gba_cpu_pipelineState)  
        case GBA_CPU_PIPELINESTATE_FLUSH 
            gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_FETCH 
             
        case GBA_CPU_PIPELINESTATE_FETCH 
            gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_DECODE 
             
        case GBA_CPU_PIPELINESTATE_DECODE 
            gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_EXECUTE 
             
        case GBA_CPU_PIPELINESTATE_EXECUTE 
            gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_EXECUTE 
               
   End Select

End Sub


Function gba_cpu_checkCondition(condition As gba_cpu_condition_t) As BOOL 
	' !valor es 0 en caso postivo o negativo, y 1 en caso 0

	  'Print condition;":";
     'print gba_cpu_flagN ;
     'print gba_cpu_flagZ ;
     'print gba_cpu_flagV ;" - ";
     'print gba_cpu_flagC ;
     'print gba_cpu_flagI ;
     'print gba_cpu_flagF ;
     'print gba_cpu_flagT ;
     'print gba_cpu_shifterCarry
     'Print IIf( gba_cpu_flagZ   , 1, 0);
     'Print IIf( gba_cpu_flagZ=0 , 1, 0)      
     'Sleep
     
     
	'gba_cpu_flagN =	IIf( gba_cpu_flagN ,1 ,0)
	'gba_cpu_flagZ =	IIf( gba_cpu_flagZ ,1 ,0) 
	'gba_cpu_flagC =	IIf( gba_cpu_flagC ,1 ,0) 
	'gba_cpu_flagV =	IIf( gba_cpu_flagV ,1 ,0) 
	'gba_cpu_flagI =	IIf( gba_cpu_flagI ,1 ,0) 
	'gba_cpu_flagF =	IIf( gba_cpu_flagF ,1 ,0) 
	'gba_cpu_flagT =	IIf( gba_cpu_flagT ,1 ,0) 
	'gba_cpu_shifterCarry = IIf( gba_cpu_shifterCarry ,1 ,0)

    Select Case As Const   (condition)  
    	case GBA_CPU_CONDITION_EQ  
    		return IIf( gba_cpu_flagZ   , 1, 0)
    	case GBA_CPU_CONDITION_NE 
    		return IIf( gba_cpu_flagZ=0 , 1, 0) 
    	case GBA_CPU_CONDITION_CS
    		return IIf( gba_cpu_flagC   , 1, 0) 
    	case GBA_CPU_CONDITION_CC
    		return IIf( gba_cpu_flagC=0 , 1, 0) 
    	case GBA_CPU_CONDITION_MI
    		return IIf( gba_cpu_flagN   , 1, 0) 
    	case GBA_CPU_CONDITION_PL
    		return IIf( gba_cpu_flagN=0 , 1, 0) 
    	case GBA_CPU_CONDITION_VS
    		return IIf( gba_cpu_flagV   , 1, 0) 
    	case GBA_CPU_CONDITION_VC
    		return IIf( gba_cpu_flagV=0 , 1, 0) 
    	case GBA_CPU_CONDITION_HI
    		return IIf( gba_cpu_flagC And (gba_cpu_flagZ=0) , 1, 0) 
    	case GBA_CPU_CONDITION_LS
    		return IIf( (gba_cpu_flagC=0) Or gba_cpu_flagZ , 1, 0) 
    	case GBA_CPU_CONDITION_GE 
    		return IIf( gba_cpu_flagN = gba_cpu_flagV , 1, 0) 
    	case GBA_CPU_CONDITION_LT
    		return IIf( gba_cpu_flagN<> gba_cpu_flagV , 1, 0)
    	case GBA_CPU_CONDITION_GT
    		return IIf((gba_cpu_flagZ=0) And (gba_cpu_flagN = gba_cpu_flagV), 1, 0)
    	case GBA_CPU_CONDITION_LE
    		return IIf( gba_cpu_flagZ Or (gba_cpu_flagN <> gba_cpu_flagV), 1, 0) 
    	case GBA_CPU_CONDITION_AL
    		return 1 
    	Case else  
    		return 0 
    End Select

	return 0 ' ne deberia llegar aqui, pero por si acaso ocurre...
End Function

Function gba_cpu_getCpsr() As uint32_t 

    return 0 _
        Or IIf(gba_cpu_flagN , 1 Shl 31 , 0)_
        Or IIf(gba_cpu_flagZ , 1 Shl 30 , 0)_
        Or IIf(gba_cpu_flagC , 1 Shl 29 , 0)_
        Or IIf(gba_cpu_flagV , 1 Shl 28 , 0)_
        Or IIf(gba_cpu_flagI , 1 Shl  7 , 0)_
        Or IIf(gba_cpu_flagF , 1 Shl  6 , 0)_
        Or IIf(gba_cpu_flagT , 1 Shl  5 , 0)_
        Or gba_cpu_mode 
         
End Function

Sub gba_cpu_setCpsr(value As uint32_t) 

    gba_cpu_flagN = leeBIT(value,31) 
    gba_cpu_flagZ = leeBIT(value,30)
    gba_cpu_flagC = leeBIT(value,29)
    gba_cpu_flagV = leeBIT(value,28)
    gba_cpu_flagI = leeBIT(value,7)
    gba_cpu_flagF = leeBIT(value,6) 
    gba_cpu_flagT = leeBIT(value,5)

    gba_cpu_changeMode(value And &h1f) 
End Sub

Function gba_cpu_getSpsr() As uint32_t 
	
    Select Case As Const   (gba_cpu_mode)  
        case GBA_CPU_MODE_FIQ_OLD ,_
         	 GBA_CPU_MODE_FIQ 
            return gba_cpu_spsr_fiq 

        case GBA_CPU_MODE_IRQ_OLD ,_
         	 GBA_CPU_MODE_IRQ 
            return gba_cpu_spsr_irq 

        case GBA_CPU_MODE_SVC_OLD ,_
         	 GBA_CPU_MODE_SVC 
            return gba_cpu_spsr_svc 

        case GBA_CPU_MODE_ABT 
            return gba_cpu_spsr_abt 

        case GBA_CPU_MODE_UND 
            return gba_cpu_spsr_und 

        case else 
            return gba_cpu_getCpsr() 
    End Select
    
End Function

Sub gba_cpu_setSpsr(value As uint32_t) 
	
    Select Case As Const   (gba_cpu_mode)  
        case GBA_CPU_MODE_FIQ_OLD ,_
         	 GBA_CPU_MODE_FIQ 
            gba_cpu_spsr_fiq = value 

        case GBA_CPU_MODE_IRQ_OLD ,_
         	 GBA_CPU_MODE_IRQ 
            gba_cpu_spsr_irq = value 

        case GBA_CPU_MODE_SVC_OLD ,_
         	 GBA_CPU_MODE_SVC 
            gba_cpu_spsr_svc = value 

        case GBA_CPU_MODE_ABT 
            gba_cpu_spsr_abt = value 

        case GBA_CPU_MODE_UND 
            gba_cpu_spsr_und = value 

        case else 
            gba_cpu_setCpsr(value) 
    End Select

End Sub

Sub gba_cpu_changeMode(newMode As gba_cpu_mode_t) 
    Dim As gba_cpu_mode_t oldMode = gba_cpu_mode 

    Select Case As Const   (oldMode)  
    	case GBA_CPU_MODE_USR_OLD ,_
         GBA_CPU_MODE_USR ,_
         GBA_CPU_MODE_SYS 
            gba_cpu_r_usr(0) = gba_cpu_r(8) 
            gba_cpu_r_usr(1) = gba_cpu_r(9) 
            gba_cpu_r_usr(2) = gba_cpu_r(10) 
            gba_cpu_r_usr(3) = gba_cpu_r(11) 
            gba_cpu_r_usr(4) = gba_cpu_r(12) 
            gba_cpu_r_usr(5) = gba_cpu_r(13) 
            gba_cpu_r_usr(6) = gba_cpu_r(14) 

        case GBA_CPU_MODE_FIQ_OLD ,_
         GBA_CPU_MODE_FIQ 
            gba_cpu_r_fiq(0) = gba_cpu_r(8) 
            gba_cpu_r_fiq(1) = gba_cpu_r(9) 
            gba_cpu_r_fiq(2) = gba_cpu_r(10) 
            gba_cpu_r_fiq(3) = gba_cpu_r(11) 
            gba_cpu_r_fiq(4) = gba_cpu_r(12) 
            gba_cpu_r_fiq(5) = gba_cpu_r(13) 
            gba_cpu_r_fiq(6) = gba_cpu_r(14) 

        case GBA_CPU_MODE_IRQ_OLD ,_
         GBA_CPU_MODE_IRQ 
            gba_cpu_r_usr(0) = gba_cpu_r(8) 
            gba_cpu_r_usr(1) = gba_cpu_r(9) 
            gba_cpu_r_usr(2) = gba_cpu_r(10) 
            gba_cpu_r_usr(3) = gba_cpu_r(11) 
            gba_cpu_r_usr(4) = gba_cpu_r(12) 
            gba_cpu_r_irq(0) = gba_cpu_r(13) 
            gba_cpu_r_irq(1) = gba_cpu_r(14) 

        case GBA_CPU_MODE_SVC_OLD ,_
         GBA_CPU_MODE_SVC 
            gba_cpu_r_usr(0) = gba_cpu_r(8) 
            gba_cpu_r_usr(1) = gba_cpu_r(9) 
            gba_cpu_r_usr(2) = gba_cpu_r(10) 
            gba_cpu_r_usr(3) = gba_cpu_r(11) 
            gba_cpu_r_usr(4) = gba_cpu_r(12) 
            gba_cpu_r_svc(0) = gba_cpu_r(13) 
            gba_cpu_r_svc(1) = gba_cpu_r(14) 

        case GBA_CPU_MODE_ABT 
            gba_cpu_r_usr(0) = gba_cpu_r(8) 
            gba_cpu_r_usr(1) = gba_cpu_r(9) 
            gba_cpu_r_usr(2) = gba_cpu_r(10) 
            gba_cpu_r_usr(3) = gba_cpu_r(11) 
            gba_cpu_r_usr(4) = gba_cpu_r(12) 
            gba_cpu_r_abt(0) = gba_cpu_r(13) 
            gba_cpu_r_abt(1) = gba_cpu_r(14) 

        case GBA_CPU_MODE_UND 
            gba_cpu_r_usr(0) = gba_cpu_r(8) 
            gba_cpu_r_usr(1) = gba_cpu_r(9) 
            gba_cpu_r_usr(2) = gba_cpu_r(10) 
            gba_cpu_r_usr(3) = gba_cpu_r(11) 
            gba_cpu_r_usr(4) = gba_cpu_r(12) 
            gba_cpu_r_und(0) = gba_cpu_r(13) 
            gba_cpu_r_und(1) = gba_cpu_r(14) 
             
    End Select


    Select Case As Const   (newMode)  
        case GBA_CPU_MODE_USR_OLD ,_
         GBA_CPU_MODE_USR ,_
         GBA_CPU_MODE_SYS 
            gba_cpu_r(8)  = gba_cpu_r_usr(0) 
            gba_cpu_r(9)  = gba_cpu_r_usr(1) 
            gba_cpu_r(10) = gba_cpu_r_usr(2) 
            gba_cpu_r(11) = gba_cpu_r_usr(3) 
            gba_cpu_r(12) = gba_cpu_r_usr(4) 
            gba_cpu_r(13) = gba_cpu_r_usr(5) 
            gba_cpu_r(14) = gba_cpu_r_usr(6) 

        case GBA_CPU_MODE_FIQ_OLD ,_
         GBA_CPU_MODE_FIQ 
            gba_cpu_r(8)  = gba_cpu_r_fiq(0) 
            gba_cpu_r(9)  = gba_cpu_r_fiq(1) 
            gba_cpu_r(10) = gba_cpu_r_fiq(2) 
            gba_cpu_r(11) = gba_cpu_r_fiq(3) 
            gba_cpu_r(12) = gba_cpu_r_fiq(4) 
            gba_cpu_r(13) = gba_cpu_r_fiq(5) 
            gba_cpu_r(14) = gba_cpu_r_fiq(6) 

        case GBA_CPU_MODE_IRQ_OLD ,_
         GBA_CPU_MODE_IRQ 
            gba_cpu_r(8)  = gba_cpu_r_usr(0) 
            gba_cpu_r(9)  = gba_cpu_r_usr(1) 
            gba_cpu_r(10) = gba_cpu_r_usr(2) 
            gba_cpu_r(11) = gba_cpu_r_usr(3) 
            gba_cpu_r(12) = gba_cpu_r_usr(4) 
            gba_cpu_r(13) = gba_cpu_r_irq(0) 
            gba_cpu_r(14) = gba_cpu_r_irq(1) 

        case GBA_CPU_MODE_SVC_OLD ,_
         GBA_CPU_MODE_SVC 
            gba_cpu_r(8)  = gba_cpu_r_usr(0) 
            gba_cpu_r(9)  = gba_cpu_r_usr(1) 
            gba_cpu_r(10) = gba_cpu_r_usr(2) 
            gba_cpu_r(11) = gba_cpu_r_usr(3) 
            gba_cpu_r(12) = gba_cpu_r_usr(4) 
            gba_cpu_r(13) = gba_cpu_r_svc(0) 
            gba_cpu_r(14) = gba_cpu_r_svc(1) 

        case GBA_CPU_MODE_ABT 
            gba_cpu_r(8)  = gba_cpu_r_usr(0) 
            gba_cpu_r(9)  = gba_cpu_r_usr(1) 
            gba_cpu_r(10) = gba_cpu_r_usr(2) 
            gba_cpu_r(11) = gba_cpu_r_usr(3) 
            gba_cpu_r(12) = gba_cpu_r_usr(4) 
            gba_cpu_r(13) = gba_cpu_r_abt(0) 
            gba_cpu_r(14) = gba_cpu_r_abt(1) 

        case GBA_CPU_MODE_UND 
            gba_cpu_r(8)  = gba_cpu_r_usr(0) 
            gba_cpu_r(9)  = gba_cpu_r_usr(1) 
            gba_cpu_r(10) = gba_cpu_r_usr(2) 
            gba_cpu_r(11) = gba_cpu_r_usr(3) 
            gba_cpu_r(12) = gba_cpu_r_usr(4) 
            gba_cpu_r(13) = gba_cpu_r_und(0) 
            gba_cpu_r(14) = gba_cpu_r_und(1) 
    	 ' TODO: undefined mode
    End Select

    gba_cpu_mode = newMode 
End Sub

Sub gba_cpu_performJump(addr As uint32_t) 
    addr    -= IIf(gba_cpu_flagT , 2 , 4) 
    addr And = IIf(gba_cpu_flagT , &hfffffffe , &hfffffffc) 
    gba_cpu_r(15) = addr 
    gba_cpu_pipelineState = GBA_CPU_PIPELINESTATE_FLUSH 
End Sub

Sub gba_cpu_raiseIrq() 
    gba_cpu_spsr_irq = gba_cpu_getCpsr() 
    gba_cpu_changeMode(GBA_CPU_MODE_IRQ) 
    gba_cpu_r(14) = gba_cpu_r(15) - IIf(gba_cpu_flagT , 0 , 4) 
    gba_cpu_flagT = 0 
    gba_cpu_flagI = 1 
    gba_cpu_performJump(&h00000018) 
End Sub

Sub gba_cpu_raiseSwi() 
    gba_cpu_spsr_svc = gba_cpu_getCpsr() 
    gba_cpu_changeMode(GBA_CPU_MODE_SVC) 
    gba_cpu_r(14) = gba_cpu_r(15) - IIf(gba_cpu_flagT , 2 , 4) 
    gba_cpu_flagT = 0 
    gba_cpu_flagI = 1 
    gba_cpu_performJump(&h00000008) 
End Sub

Sub gba_cpu_raiseUnd() 
    gba_cpu_spsr_und = gba_cpu_getCpsr() 
    gba_cpu_changeMode(GBA_CPU_MODE_UND) 
    gba_cpu_r(14) = gba_cpu_r(15) - IIf(gba_cpu_flagT , 2 , 4) 
    gba_cpu_flagT = 0 
    gba_cpu_flagI = 1 
    gba_cpu_performJump(&h00000004) 
End Sub

Sub gba_cpu_fetch(fetchAddress As uint32_t) 
    If (gba_cpu_flagT) Then 
        gba_cpu_fetchedOpcodeThumb = gba_bus_read16(fetchAddress) 
        gba_cpu_r(15) += 2 
    Else
        gba_cpu_fetchedOpcodeArm = gba_bus_read32(fetchAddress) 
        gba_cpu_r(15) += 4 
    End If
End Sub

Sub gba_cpu_initDecodeThumb() 
	
    For i as Integer=0 To 1023         
        Dim As uint16_t opcode = i Shl 6 

        gba_cpu_decodeTable_thumb(i) = NULL 

        Select Case As Const   ((opcode And &he000) Shr 13)  
        	Case 0 
				If ((opcode And &h1800) = &h1800) Then 
					If (opcode And (1 Shl 9)) Then 
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_sub 
					else    
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_add 
					End If
				Else
					Select Case As Const   ((opcode And &h1800) Shr 11)  
						case 0  
							gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_lsl
						case 1  
							gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_lsr 
						case 2  
							gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_asr
					End Select
				End If

        	Case 1 
				Select Case As Const   ((opcode And &h1800) Shr 11)  
					case 0  
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_mov
					case 1  
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_cmp
					case 2  
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_add2
					case 3  
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_sub2
				End Select

        	Case 2 
				If (opcode And (1 Shl 12)) Then 
					If (opcode And (1 Shl 9)) Then 
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldrStrh 
					Else  
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldrStr 
					End If
				Else   
					If (opcode And (1 Shl 11)) Then 
						gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldr 
					Else
						If (opcode And (1 Shl 10)) Then 
							If ((opcode And &h0380) = &h0300) Then 
								gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_bx 
							ElseIf ((opcode And &h0300) <> &h0300) Then   
								If ((opcode And &h00c0) <> &h0000) Then 
									Select Case As Const   ((opcode And &h0300) Shr 8)  
									   Case 0  
											gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_add3
									   Case 1  
											gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_cmp3
									   Case 2  
											gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_mov2
									End Select
								End If
							End If
						Else    
							Select Case As Const   ((opcode And &h03c0) Shr 6)  
								case &h0  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_and
								case &h1  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_eor
								case &h2  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_lsl2
								case &h3  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_lsr2
								case &h4 
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_asr2
								case &h5  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_adc
								case &h6  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_sbc
								case &h7  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ror
								case &h8  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_tst
								case &h9  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_neg
								case &ha  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_cmp2
								case &hb  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_cmn
								case &hc  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_orr
								case &hd  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_mul
								case &he  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_bic
								case &hf  
									gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_mvn
							
							End Select
						End If
					End If
				End If

        	Case 3 
                gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldrStr2 

        	Case 4 
                If (opcode And (1 Shl 12)) Then 
                    gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldrStr3 
                else  
                    gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldrStrh2 
                End If

        	Case 5 
                If (opcode And (1 Shl 12)) Then 
                    If ((opcode And &h0f00) = &h0000) Then 
                        gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_add5 
                    ElseIf ((opcode And &h0600) = &h0400) Then   
                        gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_pushPop
                    End If
                Else 
                    gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_add4 
                End If

        	Case 6 
                If (opcode And (1 Shl 12)) Then 
                    If ((opcode And &h0f00) = &h0f00) Then 
                        gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_swi 
                    else 
                        gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_b 
                    End If
                Else 
                    gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_ldmStm 
                End If

        	Case 7 
                If (opcode And (1 Shl 12)) Then 
                    gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_bl 
                else  
                    If ((opcode And (1 Shl 11))=0) Then 
                        gba_cpu_decodeTable_thumb(i) = @gba_cpu_thumb_b2 
                    End If
                End If
        
        End Select
    Next

End Sub

Sub gba_cpu_initDecodeArm() 
    For i as Integer=0 To 4095         
        Dim As uint32_t opcode = ((i And &h0ff0) Shl 16) Or ((i And &h000f) Shl 4) 

        gba_cpu_decodeTable_arm(i) = NULL

        Select Case As Const   ((opcode And &h0c000000) Shr 26)  
        	Case &h0 
                If ((opcode And &h0ff000f0) = &h01200010) Then 
                    gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_bx 
                ElseIf ((opcode And &h02000090) = &h00000090) Then
                     If ((opcode And &h0fb000f0) = &h01000090) Then
                        gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_swp 
                     ElseIf ((opcode And &h0fc000f0) = &h00000090) Then    
                        gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_mul
                     ElseIf ((opcode And &h0f8000f0) = &h00800090) Then 
                        gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_mull 
                     ElseIf ((opcode And &h0e000090) = &h00000090) Then   
                        gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_halfwordSignedDataTransfer
                     End If
                ElseIf ((opcode And &h01900000) = &h01000000) Then 
                    If ( ((opcode And (1 Shl 25))<>0) Or ((opcode And &h000000f0) = &h00000000)) Then  
                        gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_psrTransfer  
                    End If
                Else
                    Select Case As Const   ((opcode And &h01e00000) Shr 21)  
                    	case &h0  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_and
                    	case &h1  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_eor
                    	case &h2  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_sub
                    	case &h3  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_rsb
                    	case &h4  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_add
                    	case &h5  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_adc
                    	case &h6  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_sbc
                    	case &h7  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_rsc
                    	case &h8  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_tst
                    	case &h9  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_teq
                    	case &ha  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_cmp
                    	case &hb  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_cmn
                    	case &hc  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_orr
                    	case &hd  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_mov
                    	case &he  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_bic
                    	case &hf  
                    		gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_mvn
                    End Select  
                End If

        	case &h1 
	            If ((opcode And &h02000010) <> &h02000010) Then 
	                gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_singleDataTransfer 
	            End If

        	case &h2 
	            If (opcode And (1 Shl 25)) Then 
	                gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_b 
	            Else
	                gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_blockDataTransfer 
	            End If

        	case &h3 
	            If ((opcode And &h03000000) = &h03000000) Then 
	                gba_cpu_decodeTable_arm(i) = @gba_cpu_arm_swi 
	            End If

        End Select

    Next
End Sub

Sub gba_cpu_arm_shift(opcode As uint32_t) 
    if (opcode And &h02000000)=0 Then 
      
        Dim As uint32_t rm   = opcode And &h0000000f 
        Dim As uint32_t rm_v = gba_cpu_r(rm) 

        if (opcode And &h00000010)=0 Then 
            Dim As Integer immediate = (opcode And &h00000f80) Shr 7 

            Select Case As Const   ((opcode And &h060) Shr 5)  
            	Case 0  ' LSL
                    If (immediate = 0) Then 
                        gba_cpu_shifterResult = rm_v 
                        gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0) 
                    Else
                        gba_cpu_shifterResult =  rm_v Shl immediate 
                        gba_cpu_shifterCarry  = (rm_v Shr (32 - immediate)) And 1 
                    End If

            	case 1  ' LSR
                    If (immediate = 0) Then 
                        gba_cpu_shifterResult = 0 
                        gba_cpu_shifterCarry  = (rm_v Shr 31) and 1 
                    else   
                        gba_cpu_shifterResult =  rm_v Shr immediate 
                        gba_cpu_shifterCarry  = (rm_v Shr (immediate - 1)) And 1 
                    End If

            	case 2  ' ASR
                    If (immediate = 0) Then 
                        gba_cpu_shifterResult = CLng(rm_v) Shr 31
                        gba_cpu_shifterCarry  = (rm_v Shr 31) and 1 
                    else 
                        gba_cpu_shifterResult = CLng(rm_v) Shr immediate 
                        gba_cpu_shifterCarry  = (rm_v Shr (immediate - 1)) And 1 
                    End If

            	case 3  ' ROR/RRX
                    If (immediate = 0) Then 
                        ' RRX
                        gba_cpu_shifterResult = IIf(gba_cpu_flagC , 1 Shl 31 , 0) Or (rm_v Shr 1) 
                        gba_cpu_shifterCarry  = rm_v And 1 
                    else  
                        ' ROR
                        gba_cpu_shifterResult = gba_cpu_util_ror32(rm_v, immediate) 
                        gba_cpu_shifterCarry  = (rm_v Shr (immediate - 1)) And 1 
                    End If
            
            End Select

        Else
                 
            If (rm = 15) Then 
                rm_v += 4 
            End If

            If ((opcode And &h00000080) <> 0) Then 
                gba_cpu_raiseUnd() 
            End If
 
            Dim As uint32_t rs = (opcode And &h00000f00) Shr 8 
            Dim As uint32_t shift = gba_cpu_r(rs) And &h000000ff 

            Select Case As Const   ((opcode And &h060) Shr 5)  
            	case 0  ' LSL
                    If (shift = 0) Then 
                        gba_cpu_shifterResult = rm_v 
                        gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0)
                    ElseIf (shift < 32) Then   
                        gba_cpu_shifterResult =  rm_v Shl shift
                        gba_cpu_shifterCarry  = (rm_v Shr (32 - shift)) And 1 
                    ElseIf (shift = 32) Then 
                        gba_cpu_shifterResult = 0 
                        gba_cpu_shifterCarry  = rm_v And 1 
                    Else
                        gba_cpu_shifterResult = 0 
                        gba_cpu_shifterCarry  = 0 
                    End If

            	case 1  ' LSR
                    If (shift = 0) Then 
                        gba_cpu_shifterResult = rm_v 
                        gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0) 
                    ElseIf (shift < 32) Then    
                        gba_cpu_shifterResult =  rm_v Shr shift
                        gba_cpu_shifterCarry  = (rm_v Shr (shift - 1)) And 1                  
                    ElseIf (shift = 32) Then 
                        gba_cpu_shifterResult = 0 
                        gba_cpu_shifterCarry  = (rm_v Shr 31) and 1 
                    Else
                        gba_cpu_shifterResult = 0 
                        gba_cpu_shifterCarry  = 0 
                    End If

            	case 2  ' ASR
                    If (shift = 0) Then 
                        gba_cpu_shifterResult = rm_v 
                        gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0) 
                    ElseIf (shift < 32) Then 
                        gba_cpu_shifterResult = CLng(rm_v) Shr shift
                        gba_cpu_shifterCarry  = (rm_v Shr (shift - 1)) And 1 
                    Else  
                        gba_cpu_shifterResult = CLng(rm_v) Shr 31 
                        gba_cpu_shifterCarry  = (rm_v Shr 31) and 1 
                    End If

            	case 3  ' ROR
                    Dim As Integer rotation = shift And &h1f 
                    If (shift = 0) Then 
                        gba_cpu_shifterResult = rm_v 
                        gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0) 
                    ElseIf (rotation = 0) Then    
                        gba_cpu_shifterResult = rm_v
                        gba_cpu_shifterCarry  = (rm_v Shr 31) and 1 
                    Else 
                        gba_cpu_shifterResult = gba_cpu_util_ror32(rm_v, rotation) 
                        gba_cpu_shifterCarry  = (rm_v Shr (rotation - 1)) And 1 
                    End If
                    
            End Select
        End If
 
    Else
             
        Dim As uint32_t rotation  = (opcode Shr 7) And &h1e 
        Dim As uint32_t immediate =  opcode And &hff 

        If (rotation) Then 
            gba_cpu_shifterResult = gba_cpu_util_ror32(immediate, rotation) 
            gba_cpu_shifterCarry  = (gba_cpu_shifterResult Shr 31) And 1 
        else   
            gba_cpu_shifterResult = immediate 
            gba_cpu_shifterCarry  = IIf(gba_cpu_flagC,1,0) 
        End If
    
    End If

    gba_cpu_shifterCarry  And=1 ' en un principio, esto no es necesario, pero prefiero tenerlo
End Sub

Function gba_cpu_util_ror32(value As uint32_t , bits As Integer) As uint32_t 
    return (value Shr bits) Or (value Shl (32 - bits)) 
End Function


Sub gba_cpu_writeRegister(rd As Integer , value As uint32_t) 
    If rd=15 Then 
        gba_cpu_performJump(value) 
    else  
        gba_cpu_r(rd) = value 
    End If
End Sub

Sub gba_cpu_setFlags_logical(result As uint32_t) 
    gba_cpu_setFlags_arithmetical(result) 
    gba_cpu_flagC = IIf(gba_cpu_shifterCarry,1,0) 
End Sub

Sub gba_cpu_setFlags_arithmetical(result As uint32_t) 
    gba_cpu_flagN = (result Shr 31) And 1
    gba_cpu_flagZ = IIf(result = 0 , 1, 0)
End Sub




Function gba_cpu_getCarry_sbc(left_ As uint32_t , right_ As uint32_t , carry As BOOL) As BOOL 
	 'Print "gran duda en gba_cpu_getCarry_sbc, revisar"
    return IIf( IIf(left_ >= right_,1,0) And IIf( (left_ - right_) >= IIf(carry=0,1,0) ,1,0 ) ,1,0)
End Function

Function gba_cpu_getCarry_sub(left_ As uint32_t , right_ As uint32_t) As BOOL 
    return IIf(left_ >= right_ ,1,0)
End Function

Function gba_cpu_getOverflow_sub(left_ As uint32_t , right_ As uint32_t , result As uint32_t) As BOOL 
    'print "gran duda en gba_cpu_getOverflow_sub, revisar"  
	return IIf( ((left_ Xor right_) And (left_ Xor result)) Shr 31 ,1,0)
End Function

Function gba_cpu_getOverflow_add(left_ As uint32_t , right_ As uint32_t , result As uint32_t) As BOOL 
    'print "gran duda en gba_cpu_getOverflow_add, revisar"
	return IIf( (((left_ Xor right_) Shr 31)=0) And (((left_ Xor result) Shr 31)<>0) , 1,0)
End Function




Function gba_cpu_util_hammingWeight16(value As uint16_t) As UInteger 
    Dim As UInteger count = 0 

    For i as Integer=0 To 15         
        If (value And (1 Shl i)) Then 
            count+=1  
        End If
    Next

    return count 
End Function

Function gba_cpu_util_hammingWeight8(value As uint8_t) As UInteger 
    Dim As uInteger count = 0 

    For i as Integer=0 To  7       
        If (value And (1 Shl i)) Then 
            count+=1  
        End If
    Next

    return count 
End Function

Sub gba_cpu_arm_halfwordSignedDataTransfer(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_halfwordSignedDataTransfer" : if DEB=3 then Sleep
    
	 Dim As BOOL p = leeBIT(opcode,24)
    Dim As BOOL u = leeBIT(opcode,23)
    Dim As BOOL i = leeBIT(opcode,22)
    Dim As BOOL w = leeBIT(opcode,21)
    Dim As BOOL l = leeBIT(opcode,20)
    Dim As BOOL s = leeBIT(opcode,6)
    Dim As BOOL h = leeBIT(opcode,5) 
    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 

    if (s=0) And (h=0) Then 
        gba_cpu_raiseUnd() 
    End If
 
    Dim As uint32_t addr = gba_cpu_r(rn) 
    Dim As uint32_t offset 

    If (i) Then 
        offset = ((opcode And &h00000f00) Shr 4) Or (opcode And &h0000000f) 
    else   
        If (((opcode And &h00000f00) Shr 8) <> 0) Then 
            gba_cpu_raiseUnd() 
        End If
        Dim As uint32_t rm = opcode And &h0000000f 
        offset = gba_cpu_r(rm) 
    End If
 

    If (p) Then 
        If (u) Then
            addr += offset 
        else   
            addr -= offset 
        End If
    Else  
        w = 1 
    End If
 
    If (l) Then 
        If (w) Then           
            If (p) Then               
                gba_cpu_r(rn) = addr 
            else                    
                If (u) Then                   
                    gba_cpu_r(rn) = addr + offset 
                Else                         
                    gba_cpu_r(rn) = addr - offset                 
                End If
            End If
        End If
 
        If (s=0) And (h<>0) Then  ' LDRH
            If (addr And 1) Then
                gba_cpu_writeRegister(rd, gba_cpu_util_ror32(gba_bus_read16(addr), 8)) 
            else   
                gba_cpu_writeRegister(rd, gba_bus_read16(addr)) 
            End If
        ElseIf (h=0) Or ((addr And 1)<>0) Then   ' LDRSB
            Dim As uint32_t readValue = gba_bus_read8(addr)
            If (readValue And (1 Shl 7)) Then 
                readValue Or= &hffffff00 
            End If
            gba_cpu_writeRegister(rd, readValue) 
        else ' LDRSH
            Dim As uint32_t readValue = gba_bus_read16(addr) 
            If (readValue And (1 Shl 15)) Then 
                readValue Or= &hffff0000 
            End If
            gba_cpu_writeRegister(rd, readValue) 
        End If
    Else
        Dim As uint32_t rd_v = gba_cpu_r(rd) 
        If rd=15 Then 
            rd_v += 4 
        End If
 
        if (s) Then 
            gba_cpu_raiseUnd() 
        Else ' STRH
            gba_bus_write16(addr, rd_v) 
        End If
 
        If (w) Then 
            If (p) Then 
                gba_cpu_r(rn) = addr 
            Else     
                If (u) Then 
                    gba_cpu_r(rn) = addr + offset 
                else    
                    gba_cpu_r(rn) = addr - offset 
                End If
            End If
        End If
    End If
 
End Sub
static shared aaa As UInteger
Sub gba_cpu_arm_blockDataTransfer(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_blockDataTransfer" : if DEB=3 then Sleep
    Dim As BOOL p = leeBIT(opcode,24)
    Dim As BOOL u = leeBIT(opcode,23)
    Dim As BOOL s = leeBIT(opcode,22)
    Dim As BOOL w = leeBIT(opcode,21)
    Dim As BOOL l = leeBIT(opcode,20)
    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 
    Dim As uint32_t registerCount = gba_cpu_util_hammingWeight16(opcode) 
    Dim As uint16_t rlist = opcode 

    Dim As uint32_t addr = rn_v 

    If rlist=0 Then 
        registerCount = 16 
    End If
 
    If (p) Then 
        If (u) Then 
            addr += 4 
        else      
            addr -= 4 * registerCount 
        End If
    Else   
        if u=0 Then 
            addr -= 4 * (registerCount - 1) 
        End If
    End If
 

    If rlist=0 Then 
        If (l) Then  
            gba_cpu_performJump(gba_bus_read32(addr)) 
        else   
            gba_bus_write32(addr, gba_cpu_r(15) + 4) 
        End If
 
        If (u) Then 
            gba_cpu_r(rn) += &h40 
        else  
            gba_cpu_r(rn) -= &h40 
        End If
 
    Else
             
        If (l) Then  ' LDM
            If (w) Then 
                If (u) Then 
                    gba_cpu_r(rn) = rn_v + 4 * registerCount 
                Else
                    gba_cpu_r(rn) = rn_v - 4 * registerCount 
                End If
            End If

            For i as Integer=0 To 15        
                If (opcode And (1 Shl i)) Then  
                    If (s) Then 
                        Dim As BOOL r15 = IIf(i = 15,1,0) 
                        Dim As BOOL notUserMode    = IIf( (gba_cpu_mode <> GBA_CPU_MODE_USR) And (gba_cpu_mode <> GBA_CPU_MODE_USR_OLD) And (gba_cpu_mode <> GBA_CPU_MODE_SYS), 1, 0) 
                        Dim As BOOL registerBanked = IIf( ( (((gba_cpu_mode = GBA_CPU_MODE_FIQ) Or (gba_cpu_mode = GBA_CPU_MODE_FIQ_OLD))<>0) And (i >= 8)) Or (i = 13) Or (i = 14), 1, 0) 

                        If (r15) Then 
                           gba_cpu_setCpsr(gba_cpu_getSpsr()) 
                           gba_cpu_performJump(gba_bus_read32(addr)) 
                        ElseIf (notUserMode<>0) And (registerBanked<>0) Then     
                        	' nota: el test de LDM de la ROM "armwrestler" da error aqui.
                        	' este test de ROM, lee del su propio cartucho, en la RAMDIR 0x8002498 (0x0002498 del cartucho)
                        	' el dato 0x11223344. eso es correcto, pero luego, no se que ocurre, por que el test da error
                        	' de instruccion LDMIA (LDMIB) (en el test, es el tercer menu, el de "LDM/STM"
                           gba_cpu_r_usr(i - 8) = gba_bus_read32(addr)
                        Else
                        	' idem tras el otro, pero en la RAMDIR 0x800249c (0x000249c del cartucho) y dato 0x55667788
                        	gba_cpu_r(i) = gba_bus_read32(addr)
                        End If
                    else    
                        gba_cpu_writeRegister(i, gba_bus_read32(addr)) 
                    End If
                    addr += 4 
                End If
            Next

        Else  ' STM
            Dim As BOOL firstCycle = TRUE 
            Dim As BOOL secondCycle = FALSE 

            For i as Integer=0 To 15        
                If (rlist And (1 Shl i)) Then  
                	
                    If (firstCycle) Then    
                        firstCycle = FALSE 
                        secondCycle = TRUE 
                    ElseIf (secondCycle) Then       
                        If (w) Then 
                            If (u) Then 
                                gba_cpu_r(rn) = rn_v + registerCount * 4 
                            Else
                                gba_cpu_r(rn) = rn_v - registerCount * 4
                            End If
                        End If
                        secondCycle = FALSE 
                    End If
                     
                 	If (s) Then 
                        Dim As BOOL notUserMode    = IIf( (gba_cpu_mode <> GBA_CPU_MODE_USR) And (gba_cpu_mode <> GBA_CPU_MODE_USR_OLD) And (gba_cpu_mode <> GBA_CPU_MODE_SYS) ,1 ,0)
                        Dim As BOOL registerBanked = IIf( (((gba_cpu_mode = GBA_CPU_MODE_FIQ) Or (gba_cpu_mode = GBA_CPU_MODE_FIQ_OLD)) And (i >= 8)) Or (i = 13) Or (i = 14) ,1 ,0)

                        If (notUserMode<>0) And (registerBanked<>0) Then 
                            gba_bus_write32(addr, gba_cpu_r_usr(i - 8)) 
                        else 
                            gba_bus_write32(addr, gba_cpu_r(i)) 
                        End If
                 	Else
                        If (i = 15) And (secondCycle=0) Then 
                            gba_bus_write32(addr, gba_cpu_r(15) + 4) 
                        else  
                            gba_bus_write32(addr, gba_cpu_r(i)) 
                        End If
                 	End If
                 		
                    addr += 4 
                End If
            Next 
       
            If (w<>0) And (secondCycle<>0) Then            
                If (u) Then   
                    gba_cpu_r(rn) = rn_v + 4 
                Else
                    gba_cpu_r(rn) = rn_v - 4 
                End If
            End If
        End If
        
    End If
 
End Sub

Sub gba_cpu_arm_swp(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_swp" : if DEB=3 then Sleep
    Dim As BOOL b = leeBIT(opcode,22)
    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim As uint32_t rm =  opcode And &h0000000f 

    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If (b) Then 
        Dim As uint8_t tmp = gba_bus_read8(rn_v) 
        gba_bus_write8(rn_v, gba_cpu_r(rm)) 
        gba_cpu_r(rd) = tmp 
    else  
        Dim As uint32_t tmp = gba_cpu_util_ror32(gba_bus_read32(rn_v), (rn_v And 3) Shl 3) 
        gba_bus_write32(rn_v, gba_cpu_r(rm)) 
        gba_cpu_r(rd) = tmp 
    End If
 
End Sub

Sub gba_cpu_arm_mul(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_mul" : if DEB=3 then Sleep
    Dim As BOOL a = leeBIT(opcode,21)
    Dim As BOOL s = leeBIT(opcode,20)
    Dim As uint32_t rd = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rs = (opcode And &h00000f00) Shr 8 
    Dim As uint32_t rm =  opcode And &h0000000f 

    Dim As uint32_t result = gba_cpu_r(rm) * gba_cpu_r(rs) 

    If (a) Then 
        Dim As uint32_t rn = (opcode And &h0000f000) Shr 12 
        result += gba_cpu_r(rn) 
    End If

    gba_cpu_r(rd) = result 

    If (s) Then 
        gba_cpu_setFlags_logical(result) 
    End If
 
End Sub

Sub gba_cpu_arm_mull(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_mull" : if DEB=3 then Sleep
    Dim As BOOL u = leeBIT(opcode,22)
    Dim As BOOL a = leeBIT(opcode,21)
    Dim As BOOL s = leeBIT(opcode,20)
    Dim As uint32_t rdhi = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rdlo = (opcode And &h0000f000) Shr 12 
    Dim As uint32_t rs   = (opcode And &h00000f00) Shr 8 
    Dim As uint32_t rm   = (opcode And &h0000000f)

    Dim As uint64_t result 

    If (u) Then 
        Dim As  int64_t rm_v = CLng(gba_cpu_r(rm)) ' unicas DOS variables INT64 empleadas en todo el emulador!!!!!
        Dim As  int64_t rs_v = CLng(gba_cpu_r(rs))
        result = rm_v * rs_v 
    else   
        Dim As uint64_t rm_v = gba_cpu_r(rm) 
        Dim As uint64_t rs_v = gba_cpu_r(rs) 
        result = rm_v * rs_v 
    End If

    If (a) Then 
        result += ( CULngInt(gba_cpu_r(rdhi)) Shl 32) Or gba_cpu_r(rdlo) 
    End If
 
    gba_cpu_r(rdhi) = result Shr 32 
    gba_cpu_r(rdlo) = result 

    If (s) Then 
        gba_cpu_flagZ = IIf( result = 0, 1, 0) 
        gba_cpu_flagN = IIf( result Shr 63, 1, 0) 
    End If
 
End Sub

Sub gba_cpu_arm_bx(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_bx" : if DEB=3 then Sleep
    If ((opcode And &h0ffffff0) <> &h012fff10) Then 
        gba_cpu_raiseUnd() 
    else  
        Dim As uint32_t dest = gba_cpu_r(opcode And &h0000000f) 
        gba_cpu_flagT = IIf( dest And 1 ,1,0)
        gba_cpu_performJump(dest) 
    End If
 
End Sub

Sub gba_cpu_arm_swi(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_swi" : if DEB=3 then Sleep
    UNUSED(opcode) 
    gba_cpu_raiseSwi() 
End Sub

Sub gba_cpu_arm_singleDataTransfer(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_singleDataTransfer" : if DEB=3 then Sleep
    Dim As BOOL i = leeBIT(opcode,25) 
    Dim As BOOL p = leeBIT(opcode,24)
    Dim As BOOL u = leeBIT(opcode,23)
    Dim As BOOL b = leeBIT(opcode,22)
    Dim As BOOL w = leeBIT(opcode,21)
    Dim As BOOL l = leeBIT(opcode,20)
    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    If ((l=0) And (rd = 15)) Then 
        rd_v += 4 
    End If
 
    If (i) Then 
        gba_cpu_arm_shift(opcode Xor &h02000000) 
    else    
        gba_cpu_shifterResult = opcode And &h00000fff 
    End If

    Dim As uint32_t offset = rn_v 

    If (p) Then 
        If (u) Then 
            offset += gba_cpu_shifterResult 
        Else
            offset -= gba_cpu_shifterResult 
        End If
    Else
        w = 1 
    End If

    If (w) Then 
        If (u) Then 
            If (p) Then 
                gba_cpu_writeRegister(rn, offset) 
            Else
                gba_cpu_writeRegister(rn, offset + gba_cpu_shifterResult) 
            End If
        else
            If (p) Then 
                gba_cpu_writeRegister(rn, offset) 
            Else
                gba_cpu_writeRegister(rn, offset - gba_cpu_shifterResult) 
            End If
        End If
    End If

    If (l) Then 
        If (b) Then 
            gba_cpu_writeRegister(rd, gba_bus_read8(offset)) 
        Else
            gba_cpu_writeRegister(rd, gba_cpu_util_ror32(gba_bus_read32(offset), (offset And &h00000003) Shl 3)) 
        End If
    Else  
        If (b) Then 
            gba_bus_write8 (offset, rd_v) 
        Else
            gba_bus_write32(offset, rd_v) 
        End If
    End If
 
End Sub

Sub gba_cpu_arm_psrTransfer_msr(opcode As uint32_t , spsr As BOOL , operand As uint32_t) 
    Dim As uint32_t mask = 0 

    If (opcode And (1 Shl 19)) Then mask Or= &hff000000 
    If (opcode And (1 Shl 18)) Then mask Or= &h00ff0000 
    If (opcode And (1 Shl 17)) Then mask Or= &h0000ff00 
    If (opcode And (1 Shl 16)) Then mask Or= &h000000ff 

    Dim As uint32_t registerValue 

    If (spsr) Then 
        registerValue = gba_cpu_getSpsr() 
    else 
        registerValue = gba_cpu_getCpsr() 
    End If
 
    registerValue And = Not(mask) ' ~mask
    registerValue Or= operand And mask 

    If (spsr) Then 
        gba_cpu_setSpsr(registerValue) 
    Else
        gba_cpu_setCpsr(registerValue) 
    End If
 
End Sub

Sub gba_cpu_arm_psrTransfer(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_psrTransfer" : if DEB=3 then Sleep
    Dim As BOOL msr  = leeBIT(opcode,21)
    Dim As BOOL spsr = leeBIT(opcode,22)

    If (msr) Then 
      
        Dim As BOOL immediate = leeBIT(opcode,25) 

        If (immediate) Then 
            Dim As uint32_t shift = (opcode And &h00000f00) Shr 8 
            Dim As uint32_t imm   =  opcode And &h000000ff 
            Dim As uint32_t operand = gba_cpu_util_ror32(imm, shift Shl 1) 
            gba_cpu_arm_psrTransfer_msr(opcode, spsr, operand) 
        Else
            If ((opcode And &h00000ff0) <> &h00000000) Then 
                gba_cpu_raiseUnd() 
            End If
            Dim As uint32_t rm = opcode And &h0000000f 
            gba_cpu_arm_psrTransfer_msr(opcode, spsr, gba_cpu_r(rm))        
        End If
 
    Else
             
        If ((opcode And &h000f0fff) <> &h000f0000) Then 
            gba_cpu_raiseUnd() 
        End If
 
        Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
        If (spsr) Then 
            gba_cpu_r(rd) = gba_cpu_getSpsr() 
        Else    
            gba_cpu_r(rd) = gba_cpu_getCpsr() 
        End If

    End If
 
End Sub

Sub gba_cpu_arm_and(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_and" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn   = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v And gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_setFlags_logical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_eor(opcode As uint32_t) 
  If DEB>1 then Print hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_eor" : if DEB=3 then sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v Xor gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_setFlags_logical(result) 
        End If
    End If
 

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_sub(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_sub" : if DEB=3 then Sleep

    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 ' rn=register number (numero de registro Rx)
    Dim As uint32_t rn_v = gba_cpu_r(rn) ' rn_v= register number value (valor en si mismo de la variable9

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v - gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_flagC = gba_cpu_getCarry_sub(rn_v, gba_cpu_shifterResult) 
            gba_cpu_flagV = gba_cpu_getOverflow_sub(rn_v, gba_cpu_shifterResult, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_rsb(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_rsb" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = gba_cpu_shifterResult - rn_v 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_flagC = gba_cpu_getCarry_sub(gba_cpu_shifterResult, rn_v) 
            gba_cpu_flagV = gba_cpu_getOverflow_sub(gba_cpu_shifterResult, rn_v, result) 
            gba_cpu_setFlags_arithmetical(result) 
        
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_add(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_add" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v + gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_flagC = IIf(result < rn_v, 1, 0) 
            gba_cpu_flagV = gba_cpu_getOverflow_add(rn_v, gba_cpu_shifterResult, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_adc(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_adc" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

	 'nota: quizas CULngInt deba ser solo "CUInt"
    Dim As uint64_t result = CULngInt(rn_v) + CULngInt(gba_cpu_shifterResult) + IIf(gba_cpu_flagC,1,0) 
	
    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else        		
            gba_cpu_flagC = IIf(result > UINT32_MAX, 1, 0)
            gba_cpu_flagV = gba_cpu_getOverflow_add(rn_v, gba_cpu_shifterResult, result)
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
  
End Sub

Sub gba_cpu_arm_sbc(opcode As uint32_t) 

  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_sbc" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint64_t result = CULngInt(rn_v) - CULngInt(gba_cpu_shifterResult) - IIf(gba_cpu_flagC=0, 1, 0)

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        Else
            gba_cpu_flagC = gba_cpu_getCarry_sbc(rn_v, gba_cpu_shifterResult, gba_cpu_flagC) 
            gba_cpu_flagV = gba_cpu_getOverflow_sub(rn_v, gba_cpu_shifterResult, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_rsc(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_rsc" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint64_t result = CULngInt(gba_cpu_shifterResult) - CULngInt(rn_v) -  IIf(gba_cpu_flagC=0, 1, 0)

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else    
            gba_cpu_flagC = gba_cpu_getCarry_sbc(gba_cpu_shifterResult, rn_v, gba_cpu_flagC) 
            gba_cpu_flagV = gba_cpu_getOverflow_sub(gba_cpu_shifterResult, rn_v, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If
 
    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_tst(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_tst" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v And gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_setFlags_logical(result) 
        End If
    End If
 
End Sub

Sub gba_cpu_arm_teq(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_teq" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v Xor gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        Else
            gba_cpu_setFlags_logical(result) 
        End If
    End If
 
End Sub

Sub gba_cpu_arm_cmp(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_cmp" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v - gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_flagC = gba_cpu_getCarry_sub(rn_v, gba_cpu_shifterResult) 
            gba_cpu_flagV = gba_cpu_getOverflow_sub(rn_v, gba_cpu_shifterResult, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If
 
End Sub

Sub gba_cpu_arm_cmn(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_cmn" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v + gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        Else
            gba_cpu_flagC = IIf( result < rn_v, 1,0) 
            gba_cpu_flagV = gba_cpu_getOverflow_add(rn_v, gba_cpu_shifterResult, result) 
            gba_cpu_setFlags_arithmetical(result) 
        End If
    End If
 
End Sub

Sub gba_cpu_arm_orr(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_orr" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If
 
    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v Or gba_cpu_shifterResult 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else   
            gba_cpu_setFlags_logical(result) 
        End If
    End If
 
    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_mov(opcode As uint32_t) 
  	 If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_mov" : if DEB=3 then Sleep

    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_setFlags_logical(gba_cpu_shifterResult) 
        End If
    End If

    gba_cpu_writeRegister(rd, gba_cpu_shifterResult) 
End Sub

Sub gba_cpu_arm_bic(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_bic" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rn = (opcode And &h000f0000) Shr 16 
    Dim As uint32_t rn_v = gba_cpu_r(rn) 

    If rn=15 Then 
        If ((opcode And (1 Shl 25))=0) And ((opcode And (1 Shl 4))<>0) Then 
            rn_v += 4 
        End If
    End If

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result = rn_v And NOT(gba_cpu_shifterResult) 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else     
            gba_cpu_setFlags_logical(result) 
        End If
    End If

    gba_cpu_writeRegister(rd, result) 
End Sub

' Move Negative Register (MVN)
Sub gba_cpu_arm_mvn(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_mvn" : if DEB=3 then Sleep
    gba_cpu_arm_shift(opcode) 

    Dim As uint32_t rd = (opcode And &h0000f000) Shr 12 
    Dim as BOOL s = leeBIT(opcode,20)

    Dim As uint32_t result =  NOT(gba_cpu_shifterResult) 

    If (s) Then 
        If rd=15 Then 
            gba_cpu_setCpsr(gba_cpu_getSpsr()) 
        else  
            gba_cpu_setFlags_logical(result) 
        End If
    End If
 
    gba_cpu_writeRegister(rd, result) 
End Sub

Sub gba_cpu_arm_b(opcode As uint32_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_arm_b" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,24)

    If (l) Then 
        gba_cpu_r(14) = gba_cpu_r(15) - 4 
    End If

    Dim As uint32_t offset = (opcode And &h00ffffff) Shl 2 

    If (offset And (1 Shl 25)) Then 
        offset Or= &hfc000000 
    End If
 
    gba_cpu_performJump(gba_cpu_r(15) + offset) 
End Sub


















' ======================================================================================================
' ======================================================================================================
' ======================================================================================================
' ==============================      THUMB MODE                  ======================================
' ======================================================================================================
' ======================================================================================================
' ======================================================================================================




Sub gba_cpu_thumb_sub(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_sub" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 
    Dim as BOOL i = leeBIT(opcode,10) 
    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    Dim As uint32_t op2 

    If (i) Then 
        op2 = (opcode And &h01c0) Shr 6 
    else  
        Dim As uint16_t rn = (opcode And &h01c0) Shr 6 
        op2 = gba_cpu_r(rn) 
    End If
 
    Dim As uint32_t result = rs_v - op2 

    gba_cpu_flagC = gba_cpu_getCarry_sub(rs_v, op2) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rs_v, op2, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_add(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_add" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 
    Dim as BOOL i = leeBIT(opcode,10) 
    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    Dim As uint32_t op2 

    If (i) Then 
        op2 = (opcode And &h01c0) Shr 6 
    else  
        Dim As uint16_t rn = (opcode And &h01c0) Shr 6 
        op2 = gba_cpu_r(rn) 
    End If
 
    Dim As uint32_t result = rs_v + op2 

    gba_cpu_flagC = IIf(result < op2,1,0) 
    gba_cpu_flagV = gba_cpu_getOverflow_add(rs_v, op2, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_lsl(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_lsl" : if DEB=3 then Sleep
    Dim As uint16_t offset = (opcode And &h07c0) Shr 6 
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    If (offset) Then 
        gba_cpu_r(rd) = rs_v Shl offset 
        gba_cpu_flagC = IIf(rs_v Shr (32 - offset),1,0) 
    else   
        gba_cpu_r(rd) = rs_v 
    End If

    gba_cpu_setFlags_arithmetical(gba_cpu_r(rd)) 
End Sub

Sub gba_cpu_thumb_lsr(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_lsr" : if DEB=3 then Sleep
    Dim As uint16_t offset = (opcode And &h07c0) Shr 6 
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    If (offset) Then 
        gba_cpu_r(rd) =  rs_v Shr  offset 
        gba_cpu_flagC = (rs_v Shr (offset - 1)) And 1
    else   
        gba_cpu_r(rd) = 0 
        gba_cpu_flagC = (rs_v Shr 31) and 1
    End If

    gba_cpu_setFlags_arithmetical(gba_cpu_r(rd)) 
End Sub

Sub gba_cpu_thumb_mov(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_mov" : if DEB=3 then Sleep
    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t offset = opcode And &h00ff 

    gba_cpu_r(rd) = offset 

    gba_cpu_flagZ = IIf( offset = 0, 1, 0) 
    gba_cpu_flagN = 0 
End Sub

Sub gba_cpu_thumb_cmp(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_cmp" : if DEB=3 then Sleep
    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t offset = opcode And &h00ff 

    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v - offset 

    gba_cpu_flagC = gba_cpu_getCarry_sub(rd_v, offset) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rd_v, offset, result) 
    gba_cpu_setFlags_arithmetical(result) 
End Sub

Sub gba_cpu_thumb_add2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_add2" : if DEB=3 then Sleep
    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t offset = opcode And &h00ff 

    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v + offset 

    gba_cpu_flagC = IIf( result < offset , 1, 0) 
    gba_cpu_flagV = gba_cpu_getOverflow_add(rd_v, offset, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 

End Sub

Sub gba_cpu_thumb_sub2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_sub2" : if DEB=3 then Sleep
    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t offset = opcode And &h00ff 

    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v - offset 

    gba_cpu_flagC = gba_cpu_getCarry_sub(rd_v, offset) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rd_v, offset, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_ldrStrh(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldrStrh" : if DEB=3 then Sleep
    Dim as BOOL h = leeBIT(opcode,11) 
    Dim as BOOL s = leeBIT(opcode,10) 
    Dim As uint16_t ro = (opcode And &h01c0) Shr 6 
    Dim As uint16_t rb = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t ro_v = gba_cpu_r(ro) 
    Dim As uint32_t rb_v = gba_cpu_r(rb) 

    Dim As uint32_t offset = ro_v + rb_v 

    If (s) Then 
        If (h<>0) And ((offset And 1)=0) Then 
            Dim As uint32_t result = gba_bus_read16(offset) 

            If (result And (1 Shl 15)) Then 
                result Or= &hffff0000 
            End If

            gba_cpu_r(rd) = result 
        Else
                 
            Dim As uint32_t result = gba_bus_read8(offset) 
            If (result And (1 Shl 7)) Then 
                result Or= &hffffff00 
            End If
            gba_cpu_r(rd) = result 
        End If
 
      else
             
        If (h) Then 
            gba_cpu_r(rd) = gba_cpu_util_ror32(gba_bus_read16(offset), (offset And 1) Shl 3) 
        else   
            Dim As uint32_t rd_v = gba_cpu_r(rd) 
            gba_bus_write16(offset, rd_v) 
        End If
    End If
 
End Sub

Sub gba_cpu_thumb_ldrStr(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldrStr" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,11) 
    Dim as BOOL b = leeBIT(opcode,10) 
    Dim As uint16_t ro = (opcode And &h01c0) Shr 6 
    Dim As uint16_t rb = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t ro_v = gba_cpu_r(ro) 
    Dim As uint32_t rb_v = gba_cpu_r(rb) 

    Dim As uint32_t offset = ro_v + rb_v 

    If (l) Then 
        If (b) Then 
            gba_cpu_r(rd) = gba_bus_read8(offset) 
        else 
            gba_cpu_r(rd) = gba_cpu_util_ror32(gba_bus_read32(offset), (offset And &h00000003) Shl 3) 
        End If
 
      else
             
        Dim As uint32_t rd_v = gba_cpu_r(rd) 

        If (b) Then 
            gba_bus_write8(offset, rd_v) 
        else   
            gba_bus_write32(offset, rd_v) 
        End If
 
    End If
 
End Sub

Sub gba_cpu_thumb_ldr(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldr" : if DEB=3 then Sleep
    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t immediate = opcode And &h00ff 
    Dim As uint32_t result = (gba_cpu_r(15) And &hfffffffc) + (immediate Shl 2) 

    gba_cpu_r(rd) = gba_bus_read32(result) 
End Sub

Sub gba_cpu_thumb_bx(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_bx" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 

    If (opcode And (1 Shl 6)) Then 
        rs += 8 
    End If
 
    Dim As uint32_t dest = gba_cpu_r(rs) 

    gba_cpu_flagT = (dest And 1)
    gba_cpu_performJump(dest) 
End Sub

Sub gba_cpu_thumb_add3(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_add3" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    If (opcode And (1 Shl 7)) Then 
        rd += 8 
    End If
 
    If (opcode And (1 Shl 6)) Then 
        rs += 8 
    End If
 
    gba_cpu_writeRegister(rd, gba_cpu_r(rd) + gba_cpu_r(rs)) 
End Sub

Sub gba_cpu_thumb_cmp3(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_cmp3" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    If (opcode And (1 Shl 7)) Then 
        rd += 8 
    End If
 
    If (opcode And (1 Shl 6)) Then 
        rs += 8 
    End If
 
    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v - rs_v 
    gba_cpu_flagC = gba_cpu_getCarry_sub(rd_v, rs_v) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rd_v, rs_v, result) 
    gba_cpu_setFlags_arithmetical(result) 
End Sub

Sub gba_cpu_thumb_mov2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_mov2" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    If (opcode And (1 Shl 7)) Then 
        rd += 8 
    End If
 
    If (opcode And (1 Shl 6)) Then 
        rs += 8 
    End If
 
    gba_cpu_writeRegister(rd, gba_cpu_r(rs)) 
End Sub

Sub gba_cpu_thumb_and(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_and" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rs_v And rd_v 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_eor(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_eor" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rs_v Xor rd_v 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_lsl2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_lsl2" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result 

    If rs_v=0 Then 
        result = rd_v 
    ElseIf (rs_v < 32) Then    
        result = rd_v Shl rs_v
        gba_cpu_flagC = (rd_v Shr (32 - rs_v)) And 1
    ElseIf (rs_v = 32) Then 
        result = 0 
        gba_cpu_flagC = rd_v And 1
    else  
        result = 0 
        gba_cpu_flagC = 0 
    End If

    gba_cpu_setFlags_arithmetical(result) 
    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_lsr2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_lsr2" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result 

    If rs_v=0 Then 
        result = rd_v 
    ElseIf (rs_v < 32) Then   
        result = rd_v Shr rs_v
        gba_cpu_flagC = (rd_v Shr (rs_v - 1)) And 1
    ElseIf (rs_v = 32) Then 
        result = 0 
        gba_cpu_flagC = (rd_v Shr 31) and 1
    else   
        result = 0 
        gba_cpu_flagC = 0 
    End If
 
    gba_cpu_setFlags_arithmetical(result) 
    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_asr(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_asr" : if DEB=3 then Sleep
    Dim As uint16_t offset = (opcode And &h07c0) Shr 6 
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    If (offset) Then 
        gba_cpu_r(rd) = CLng(rs_v) Shr offset
        gba_cpu_flagC = (rs_v Shr (offset - 1)) And 1
    else 
        gba_cpu_r(rd) = CLng(rs_v) Shr 31 
        gba_cpu_flagC = (rs_v Shr 31) and 1
    End If

    gba_cpu_setFlags_arithmetical(gba_cpu_r(rd)) 
End Sub

Sub gba_cpu_thumb_asr2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_asr2" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) ' rs=register source   rs_v=register source value
    Dim As uint32_t rd_v = gba_cpu_r(rd) ' rd=register destine  rd_v=register destine value

    Dim As uint32_t result 

    If rs_v=0 Then 
        result = rd_v 
    ElseIf (rs_v < 32) Then  
        result = CLng(rd_v) Shr rs_v
        gba_cpu_flagC = (rd_v Shr (rs_v - 1)) And 1
    Else 
        result = CLng(rd_v) Shr 31 
        gba_cpu_flagC =(rd_v Shr 31) and 1
    End If

    gba_cpu_setFlags_arithmetical(result) 
    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_adc(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_adc" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint64_t result = CULngInt(rd_v) + CULngInt(rs_v) + IIf( gba_cpu_flagC,1,0) 
	 
    gba_cpu_flagC = IIf(result > UINT32_MAX, 1, 0)

    gba_cpu_flagV = gba_cpu_getOverflow_add(rd_v, rs_v, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_sbc(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_sbc" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint64_t result = CULngInt(rd_v) - CULngInt(rs_v) - IIf(gba_cpu_flagC=0,1,0) 

    gba_cpu_flagC = gba_cpu_getCarry_sbc(rd_v, rs_v, gba_cpu_flagC) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rd_v, rs_v, result) 
    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_ror(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ror" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result 
    Dim As Integer rotation = rs_v And &h1f 

    If rs_v=0 Then 
        result = rd_v 
    ElseIf (rotation = 0) Then    
        result = rd_v
        gba_cpu_flagC = (rd_v Shr 31) and 1
    else 
        result = gba_cpu_util_ror32(rd_v, rotation) 
        gba_cpu_flagC = (rd_v Shr (rotation - 1)) And 1
    End If

    gba_cpu_setFlags_arithmetical(result) 
    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_tst(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_tst" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rs_v And rd_v 

    gba_cpu_setFlags_arithmetical(result) 
End Sub

Sub gba_cpu_thumb_neg(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_neg" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t result = -rs_v 
    
    gba_cpu_flagC = gba_cpu_getCarry_sub(0, rs_v) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(0, rs_v, result) 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_cmp2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_cmp2" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v - rs_v 

    gba_cpu_flagC = gba_cpu_getCarry_sub(rd_v, rs_v) 
    gba_cpu_flagV = gba_cpu_getOverflow_sub(rd_v, rs_v, result) 
    gba_cpu_setFlags_arithmetical(result) 
End Sub

Sub gba_cpu_thumb_cmn(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_cmn" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v + rs_v 

    gba_cpu_flagC = IIf( result < rd_v ,1,0)
    gba_cpu_flagV = gba_cpu_getOverflow_add(rd_v, rs_v, result) 
    gba_cpu_setFlags_arithmetical(result) 
End Sub

Sub gba_cpu_thumb_orr(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_orr" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rs_v Or rd_v 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_mul(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_mul" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rs_v * rd_v 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_bic(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_bic" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 
    Dim As uint32_t rd_v = gba_cpu_r(rd) 

    Dim As uint32_t result = rd_v And NOT(rs_v) 

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

' Move Negative Register (MVN)
Sub gba_cpu_thumb_mvn(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_mvn" : if DEB=3 then Sleep
    Dim As uint16_t rs = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd =  opcode And &h0007 

    Dim As uint32_t rs_v = gba_cpu_r(rs) 

    Dim As uint32_t result = NOT(rs_v)

    gba_cpu_setFlags_arithmetical(result) 

    gba_cpu_r(rd) = result 
End Sub

Sub gba_cpu_thumb_ldrStr2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldrStr2" : if DEB=3 then Sleep
    Dim as BOOL b = leeBIT(opcode,12) 
    Dim as BOOL l = leeBIT(opcode,11) 
    Dim As uint16_t offset = (opcode And &h07c0) Shr 6 
    Dim As uint16_t rb = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd = opcode And &h0007 

    Dim As uint32_t rb_v = gba_cpu_r(rb) 

    If (b) Then 
        Dim As uint32_t addr = rb_v + offset 
        If (l) Then     
            gba_cpu_r(rd) = gba_bus_read8(addr) 
        else      
            Dim As uint32_t rd_v = gba_cpu_r(rd) 
            gba_bus_write8(addr, rd_v) 
        End If
    Else   
        Dim As uint32_t addr = rb_v + (offset Shl 2) 
        If (l) Then   
            gba_cpu_r(rd) = gba_cpu_util_ror32(gba_bus_read32(addr), (addr And &h00000003) Shl 3) 
        else      
            Dim As uint32_t rd_v = gba_cpu_r(rd) 
            gba_bus_write32(addr, rd_v) 
        End If
    End If
 
End Sub

Sub gba_cpu_thumb_ldrStr3(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldrStr3" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,11) 

    Dim As uint16_t rd = (opcode And &h0700) Shr 8 
    Dim As uint16_t immediate = opcode And &h00ff 

    Dim As uint32_t offset = gba_cpu_r(13) + (immediate Shl 2) 

    If (l) Then 
        gba_cpu_r(rd) = gba_cpu_util_ror32(gba_bus_read32(offset), (offset And &h00000003) Shl 3) 
    else   
        Dim As uint32_t rd_v = gba_cpu_r(rd) 
        gba_bus_write32(offset, rd_v) 
    End If
 
End Sub

Sub gba_cpu_thumb_ldrStrh2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldrStrh2" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,11) 
    Dim As uint16_t offset = (opcode And &h07c0) Shr 6 
    Dim As uint16_t rb = (opcode And &h0038) Shr 3 
    Dim As uint16_t rd = opcode And &h0007 

    Dim As uint32_t rb_v = gba_cpu_r(rb) 

    Dim As uint32_t addr = rb_v + (offset Shl 1) 

    If (l) Then 
        gba_cpu_r(rd) = gba_cpu_util_ror32(gba_bus_read16(addr), (addr And &h00000001) Shl 3) 
    else     
        Dim As uint32_t rd_v = gba_cpu_r(rd) 
        gba_bus_write16(addr, rd_v) 
    End If
 
End Sub

Sub gba_cpu_thumb_add5(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_add5" : if DEB=3 then Sleep
    Dim as BOOL s = leeBIT(opcode,7) 
    Dim As uint16_t immediate = (opcode And &h007f) Shl 2 

    If (s) Then 
        gba_cpu_r(13) -= immediate 
    else    
        gba_cpu_r(13) += immediate 
    End If
 
End Sub

Sub gba_cpu_thumb_pushPop(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_pushPop" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,11) 
    Dim as BOOL r = leeBIT(opcode,8) 
    Dim As uint8_t rlist = opcode 
    Dim As uint32_t registerCount = gba_cpu_util_hammingWeight8(rlist) 

    If (r) Then 
        registerCount+=1  
    End If
 
    Dim As uint32_t addr 
    
    If (l) Then 
        addr = gba_cpu_r(13) 
        gba_cpu_r(13) += registerCount * 4 
    else 
        gba_cpu_r(13) -= registerCount * 4 
        addr = gba_cpu_r(13) 
    End If
 
    For i as Integer=0 To 7         
        If (rlist And (1 Shl i)) Then 
            If (l) Then 
                gba_cpu_r(i) = gba_bus_read32(addr) 
            else  
                gba_bus_write32(addr, gba_cpu_r(i)) 
            End If
            addr += 4 
        End If
    Next

    If (r) Then 
        If (l) Then 
            gba_cpu_performJump(gba_bus_read32(addr)) 
        else 
            gba_bus_write32(addr, gba_cpu_r(14)) 
        End If
    End If
 
End Sub

Sub gba_cpu_thumb_add4(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_add4" : if DEB=3 then Sleep
    Dim as uint16_t sp = (opcode And (1 Shl 11)) 
    Dim As uint16_t rd = (opcode And &h07c0) Shr 8 
    Dim As uint16_t immediate = opcode And &h00ff 

    If (sp) Then 
        gba_cpu_r(rd) = gba_cpu_r(13) + (immediate Shl 2) 
    else 
        gba_cpu_r(rd) = (gba_cpu_r(15) And &hfffffffc) + (immediate Shl 2) 
    End If
 
End Sub

Sub gba_cpu_thumb_swi(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_swi" : if DEB=3 then Sleep
    UNUSED(opcode) 
    gba_cpu_raiseSwi() 
End Sub

Sub gba_cpu_thumb_b(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_b" : if DEB=3 then Sleep
    Dim As gba_cpu_condition_t condition = (opcode And &h0f00) Shr 8 
    Dim As int8_t immediate = opcode 

    If (gba_cpu_checkCondition(condition)) Then 
        gba_cpu_performJump(gba_cpu_r(15) + (immediate Shl 1)) 
    End If
End Sub



static Shared As uint8_t lookupTable(7) = { _
            &h00,_
            &h01,_
            &h03,_
            &h07,_
            &h0f,_
            &h1f,_
            &h3f,_
            &h7f _
        } 
Sub gba_cpu_thumb_ldmStm(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_ldmStm" : if DEB=3 then Sleep
    Dim as BOOL l = leeBIT(opcode,11) 
    Dim As uint16_t rb = (opcode And &h0700) Shr 8 
    Dim As uint8_t rlist = opcode 

    If (rlist) Then 
        Dim As uInteger registerCount = gba_cpu_util_hammingWeight8(rlist) 

        Dim as BOOL baseInRlist = leeBIT(rlist,rb)
        Dim as BOOL baseFirstInRlist =  IIf( (rlist And lookupTable(rb))=0, 1, 0) 
        Dim As uint32_t addr = gba_cpu_r(rb) 

        If (baseInRlist<>0) And (baseFirstInRlist=0) Then 
            gba_cpu_r(rb) += registerCount * 4 
        End If
 
        For i as Integer=0 To 6        
            If (rlist And (1 Shl i)) Then 
                If (l) Then 
                    gba_cpu_r(i) = gba_bus_read32(addr) 
                else  
                    gba_bus_write32(addr, gba_cpu_r(i)) 
                End If
                addr += 4         
            End If
        Next
        gba_cpu_r(rb) = addr 
    Else
        If (l) Then 
            gba_cpu_performJump(gba_bus_read32(gba_cpu_r(rb))) 
        else   
            gba_bus_write32(gba_cpu_r(rb), gba_cpu_r(15) + 2) 
        End If
        gba_cpu_r(rb) += &h40 
    End If
 
End Sub

Sub gba_cpu_thumb_bl(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_bl" : if DEB=3 then Sleep
    Dim as BOOL h = leeBIT(opcode,11) 
    Dim As uint32_t offset = opcode And &h07ff 

    If (h) Then 
        gba_cpu_r(14) += (offset Shl 1) 
        Dim As uint32_t pc_v = gba_cpu_r(15) 
        gba_cpu_performJump(gba_cpu_r(14)) 
        gba_cpu_r(14) = (pc_v - 2) Or &h00000001 
    Else 
        If (opcode And (1 Shl 10)) Then 
            offset Or= &hfffff800 
        End If
        gba_cpu_r(14) = gba_cpu_r(15) + (offset Shl 12) 
    End If
 
End Sub

Sub gba_cpu_thumb_b2(opcode As uint16_t) 
  If DEB>1 then Print Hex(gba_cpu_r(15)-8,8),hex(opcode,8),"INS: gba_cpu_thumb_b2" : if DEB=3 then Sleep
    Dim As uint32_t immediate = opcode And &h07ff 

    If (opcode And (1 Shl 10)) Then 
        immediate Or= &hfffff800 
    End If

    gba_cpu_performJump(gba_cpu_r(15) + (immediate Shl 1)) 
End Sub
