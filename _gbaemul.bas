
' para el empleo de MULTIKEY
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#endif

' varios opcionales, usados tipicamente en "C"
#Include "crt\stdio.bi" ' printf(), scanf(), fopen(), etc
#Include "crt\stdlib.bi" ' malloc(),calloc(), etc

' SDL2
#include  "SDL2\SDL.bi"

' rutinas
#Include "variables.bas"
#Include "declaraciones.bas"
#Include "memoria.bas"
#Include "cartridge.bas"
#Include "cpu.bas"
#Include "io.bas"
#Include "ppu.bas"
#Include "timer.bas"
#Include "gba.bas"
#Include "sdl2.bas"



static shared As Byte Ptr biosPath 
static shared As Byte Ptr romPath 

static shared As Integer Ptr biosBuffer  
static shared As Integer Ptr romBuffer  
static shared As size_t romBufferSize 
static shared As Integer Ptr sramBuffer  
static shared As size_t sramBufferSize 




Function loadBios(nombre As string) As Integer
    'Dim As Long fileSize = GBA_BIOS_FILE_SIZE 
    
      posfil=0
      sa=Space(1024)
      Open nombre For Binary Access Read As 1
      	While Not Eof(1)
      		Get #1,posfil+1,sa
      		For f=1 To Len(sa)
      			bios(posfil+f-1)=Asc(Mid(sa,f,1))
      		Next
      		posfil+=Len(sa)
      	Wend
      Close 1
      
      'biosBuffer = @bios(0)
 
    return 0 
End Function



Function loadRom(nombre As string) As Integer 
    'Dim As Long fileSize = GBA_MAX_ROM_FILE_SIZE 

      posfil=0
      sa=Space(1024)
      Open nombre For Binary Access Read As 1
      	While Not Eof(1)
      		Get #1,posfil+1,sa
      		For f=1 To Len(sa)
      			rom(posfil+f-1)=Asc(Mid(sa,f,1))
      		Next
      		posfil+=Len(sa)
      	Wend
      Close 1

    'romBufferSize = fileSize 

    return 0 
End Function



' =======================================================
    sc=Command
    If sc="" Then 

	    loadRom("test\hello.gba")
	    'loadRom("test\stripes.gba")
	    'loadRom("test\shades.gba")

    Else
    	loadRom(sc)
    End If

	 ' reinicia la SRAM, solo para pruebas
	 gba_sram_reset() ' pruebas
	
    'loadBios("bios\GBA_BIOS.bin") ' rom original GBA
    loadBios("bios\BIOS_NORMMATT.bin") ' rom alternativa de "NORMMATT"

    frontend_init()
    gba_init(1) ' 1=saltar logo inicial en BIOS, 0=inicio normal, pero la BIOS orig. se cuelga, la alternativa no
    gba_setBios(@bios(0)) 

    gba_setRom(@rom(0), GBA_MAX_ROM_FILE_SIZE) 'romBufferSize) 

    While salir=0
        gba_frameAdvance() 
    Wend
    
    frontend_close() 


