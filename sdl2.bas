

#define SCREEN_SCALE 2
#define WINDOW_WIDTH  (GBA_SCREEN_WIDTH  * SCREEN_SCALE)
#define WINDOW_HEIGHT (GBA_SCREEN_HEIGHT * SCREEN_SCALE)

static shared As SDL_Window  Ptr window_ 
static shared As SDL_Surface Ptr windowSurface 
static shared As SDL_Surface Ptr screenSurface 

static shared As Uint32 timestamp 
static shared As Integer frameCounter 

static shared As BOOL keys(10) 



Function frontend_init() As Integer 
    If (SDL_Init(SDL_INIT_VIDEO)) Then 
        Print "SDL_Init() failed:", SDL_GetError()
        return 1 
    End If

    window_ = SDL_CreateWindow("gbaemu", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, WINDOW_WIDTH, WINDOW_HEIGHT, SDL_WINDOW_SHOWN) 

    if window_=0 Then 
        Print "SDL_CreateWindow() failed: ", SDL_GetError() 
        return 1 
    End If
 
    windowSurface = SDL_GetWindowSurface(window_) 

    if windowSurface=0 Then 
        Print"SDL_GetWindowSurface() failed: ", SDL_GetError() 
        return 1 
    End If
 
    screenSurface = SDL_CreateRGBSurface(0, GBA_SCREEN_WIDTH, GBA_SCREEN_HEIGHT, 32, &h000000ff, &h0000ff00, &h00ff0000, &hff000000) 

    if screenSurface=0 Then 
        print"SDL_CreateRGBSurface() failed: ", SDL_GetError() 
        return 1 
    End If
 
    frameCounter = 0 
    timestamp = SDL_GetTicks() 

    for i As Integer= 0 To 9        
        keys(i) = FALSE 
    Next

    return 0 
End Function
 
Sub updateScreen( buffer As uint32_t Ptr)
    memcpy(screenSurface->pixels, buffer, GBA_SCREEN_WIDTH * GBA_SCREEN_HEIGHT * 4) 
    SDL_BlitScaled(screenSurface, NULL, windowSurface, NULL) 
    SDL_UpdateWindowSurface(window_) 
End Sub

Sub updateFps()
    Dim As uinteger milliseconds = SDL_GetTicks() 

    frameCounter+=1  

    If ((milliseconds - timestamp) >= 1000) Then 
      
        Dim As String buffer 
        buffer="gbaemu  "+Str(frameCounter)+" fps  "+ Str(Int((frameCounter * 100) / 60))+"%" 
        SDL_SetWindowTitle(window_, buffer) 

        frameCounter = 0 
        timestamp = SDL_GetTicks() 
    
    End If
 
End Sub

Sub updateKey( keysym As SDL_Keysym Ptr , pressed As BOOL)
    Select Case (keysym->sym)  
    	case SDLK_z  
    		keys(0) = pressed 
    	case SDLK_x  
    		keys(1) = pressed 
    	case SDLK_LSHIFT  
    		keys(2) = pressed 
    	case SDLK_RETURN  
    		keys(3) = pressed 
    	case SDLK_RIGHT  
    		keys(4) = pressed 
    	case SDLK_LEFT  
    		keys(5) = pressed 
    	case SDLK_UP  
    		keys(6) = pressed 
    	case SDLK_DOWN  
    		keys(7) = pressed 
    	case SDLK_v  
    		keys(8) = pressed 
    	Case SDLK_c  
    		keys(9) = pressed 
    	Case SDLK_ESCAPE  
    		salir=1 
    End Select
End Sub

Sub updateKeypad()
    Dim As SDL_Event e 

    While (SDL_PollEvent(@e))  
        Select Case As Const   (e.type)  
        	Case SDL_WINDOWEVENT
            Select Case As Const   (e.window.event)  
            	Case SDL_WINDOWEVENT_CLOSE 
               	End 
         
            End Select

        	Case SDL_KEYDOWN 
            updateKey(@e.key.keysym, TRUE) 

        	Case SDL_KEYUP 
            updateKey(@e.key.keysym, FALSE) 
        
       End Select

    Wend

    gba_keypad_update(@keys(0)) 
End Sub

Sub frontend_frame( buffer As uint32_t Ptr) 
    updateScreen(buffer) 
    updateKeypad() 
    updateFps() 
End Sub

Sub frontend_close() 
    SDL_FreeSurface(screenSurface) 
    SDL_DestroyWindow(window_) 
    SDL_Quit() 
End Sub
