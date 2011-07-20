{*

					--------------	Elysion Rendering System -------------

	The Elysion Rendering System is a next-gen 2d renderer with post-processing, lights and shadow support in a two dimensional room.
	The ESR constists of the following sub-system:


	1.						--  Elysion Shader System  --

	The Elysion Shader-System is a low-level abstraction interface for native shader access for GLSL, the Open GL Shading Language.
	In these days shaders, expecially post-processing shaders, are very important to games and visual applications, interactive and
	realtime applications.

	The ESS( Elysion Shader System) encapsulses OpenGL vertex and pixel shader functionality and provides an easy interface to 
	interact with low-level shaders and the shader files itself. The ESS has the following structure:

		* TelVertexShader:
			An abstract interface to GLSL vertex shaders, which are necassary for tranformations in a three dimensional room.
			The vertex shader is only for tranformations, special effects such lighting and shadows are possible with 
			TelPixelShader.

		* TelPixelShader:
			Represents a low level interface to GLSL pixel shaders, which are necessary for special effects and all pixel based
			operations.

		-- Both Shader interfaces are only dataholder and are not able to do advanced operations like rendering or annotations parsing --

		* TelRenderTarget:
			Implements a simple interface to OpenGL rendertargets, which are needed for advanced post-processing shaders. 
			The class itself is only a data holder and has no advanced funtionality.

		* TelShaderParser:
			Parses a shader for special annotations which are described by the user. Annotations are a simple way to store additional
			data in a shader. All annotations are JSON based.

		* TelShaderCompiler:
			The shader compiler compiles shader in realtime and creates also hash-tags for compiled shaders. The shader compiler
			reacts automatic on shader source shaders and compiles the shader again( based on sha1/crc32 ).

	
	2. 						-- Elysion FX System --

	The Elysion FX System is a high-level post-processing framework for two dimensional rooms. It's based on the ESS and consists mostly out of
	pixel shaders and rendertargets.

	The Elysion FX System has the following structure:

		* TelEffect:
			The TelEffect class is based on a JSON-based special effect file which is a container for different pixel shaders, their parameters
			and effect conditions. The TelEffect is also only a data-holder and has no advanced functionality. 

		* TelEffectPipeline:
			The effect pipeline is a connected sequence of different effects and rendertargets. It connects different special effects.

		* TelEffectAnimator:
			The effect animator is a animated transition between two effects and applies rules to specific effects. 

	
	3.						-- Elysion Renderers --

	The Elysion Renderers System is a collection of different special-case optimized renderers for shader-based objects.

		* TelShadedSprite:
			Is sprite with material information for lighting and basic shader support.

		* TelShadedScene:
			Implements a scene structure based on shaders and connects sprites with the special effects system.

		* TelAbstractRenderer:
			Is a not-implemented interface of a basic renderer and all important function. This interface is implemented
			for special cases.

		* TelForwardRenderer:
			Is a typical forward renderer which renders a scene onto the screen. 

			

	

	!Important! The hole Elysion-Rendering System is not implemented yet.
*}

unit ElysionRendering;


uses
  ElysionUtils,
  ElysionObject,
  ElysionApplication,
  ElysionTypes,
  ElysionColor,
  ElysionInput,
  ElysionTimer,
  ElysionNode,
  ElysionTexture,
  ElysionContent,
  ElysionLogger,

  {$IFDEF USE_DGL_HEADER}
  dglOpenGL,
  {$ELSE}
  gl, glu, glext,
  {$ENDIF}
  SDL,
  SDLUtils,
  //SDLTextures,

  SysUtils,
  OpenURLUtil,
  Classes;

type

        
        {* Saves all important information for an graphics device initialization *}
        TelGraphicsFormat = record
        
                        {* OpenGL Core Format Information *}
                        FDeviceIdentifier : String; // The Device Id e.g "Android" or "Linux"
                        FPlatformGLRequirements : String; //Min Requirements for the platform's gl implementation
                        FDevicePlatforms : Array of String; //All functional platforms for this device
                        
                        {* OpenGL Version Requirements *}
                        FDeviceHasGLRequirements : Boolean; // Flag for GL-Requirements
                        FMinGLVersionMinor : Integer;
                        FMinGLVersionMajor : Integer;
                        
                        {* Rendering Options *}
                        FAutoSwapBuffer : Boolean;
                        FAutoFillBackground : Boolean;
                        FUseDoubleBuffering : Boolean;
                        FUseAdvancedRendering : Boolean; // Use advanced features like shaders
                        FUseStereoRendering : Boolean; // Use stereo rendering
                        FUseOffscreenRendering : Boolean; // render into an offscreen context
                        
                        FConfigurationFile : String; // Config File
                        
                        {* Resolutions *}
                        FHasMinResolution : Boolean; 
                        FHasMaxResolution : Boolean;
                        FMinResolution : TelVector2i;
                        FMaxResolution : TelVector2i;

        end;
        
  
        {* The TelGraphicsRessource holds information about an abstract ressource based on a texture, a size and position vector
           and some multithreading flags like the lock-flag. The ressource class is the base for all higher-level graphics
           systems like sprites or particles *}
        TelGraphicsRessource = class
                protected
                        
                      FPosition : TelVector2f;
                      FSize : TelVector2f;
                      FInitialized : Boolean;
                      FRessource : TelTexture;
                      FIsLocked : Boolean; 
                      
                        
                public
                        { State Management *}
                        procedure resetProperties;
                        procedure resetRessource;
                
                        {* Ressource Management *}
                        procedure setPosition( APos : TelVector2f );
                        procedure setSize( ASize : TelVector2f );
                        procedure setRessource( ARes : TelTexture );
                        
                        {* Useful tools for multithreading *}
                        procedure lock;
                        procedure unlock;      
                        
                        {* State Informations *}
                        function getInitialized : Boolean;
                        function getRessource : TelTexture;
                        function getIsLocked : Boolean;
                        function getPosition : TelVector2f;
                        function getSize : TelVector2f;
        
        end;
        
        PelGraphicsRessource = ^TelGraphicsRessource;
        
        
        {* The RenderRessource is a base class for every texture ressource that can act 
           as an screen buffer *}
        TelRenderRessource = class(TelGraphicsRessource)    
                public
                
                        function create( AWidth, AHeight : Integer ): Boolean;virtual;
                        function create( AWidth, AHeight : Integer; AFormat : GLuint; AType : GLuint ): Boolean;virtual;
                        
                        function freeRessource : Boolean;virtual;
                
        end;
        
        
        {* TelRenderBuffer is a abstract base class for every render buffer *}
        TelRenderBuffer = class(TelGraphicsRessource)
                protected
                
                        FBuffer : GLuint;
                
                public
                
                        function create( AWidth, AHeight : Integer ): Boolean;virtual;
                        function create( AWidth, AHeight : Integer; AFormat : GLuint; AType : GLuint ): Boolean;virtual;
                        
                        function freeRessource : Boolean;virtual;
        end;
        
        
        {* The RenderTarget is a base class for every render target in the scene. *}
        TelRenderTarget = class(TelGraphicsRessource)
                protected
                
                        FBuffer : GLuint;
                        FRenderBuffers : Array of TelRenderBuffer;
                        FRenderRessource : Array of TelRenderRessource;
        
                public
                
                
                        function create( AWidth, AHeight : Integer ): Boolean;virtual;
                        function create( AWidth, AHeight : Integer; AFormat : GLuint; AType : GLuint ): Boolean;virtual;
                        
                        function freeRessource : Boolean;virtual;
        
        end;
                
                
        
        
        
        {* The TelGraphicsProgramOptions determine which use has the current graphics program and
           in which way it's executed:
                * RenderOnlyOffscreen is suited for offscreen contexts. It prevents the program from
                  rendering into the screen buffer and renders only, if a render target is specified
                  by the graphics device.
                * RenderOnlyHardware specifies a program that is only executed on hardware on not so
                  portable. On unknown systems and systems with unknown capatibilities this option will
                  be automatically ignored by the graphics device to prevent the shader from execution
                  in an unsuited environment.
                * RenderOnlyEmulated is the flag that specifies that a program runs always on the cpu
                  and is kinda emulated by the cpu. This flag is always useful for better backward
                  compatibility and compatiblity to lower spec platforms, but the results are not so good. *}
        TelGraphicsProgramOptions = ( RenderOnlyOffscreen, RenderOnlyHardware, RenderOnlyEmulated );
        
        
        {* The TelGraphicsProgram is a abstract interface to any kind of rendering effect. The graphics program only acts
           as an base class for gpu shaders or cpu driven effects and has no deeper functionality. *}
        TelGraphicsProgram = class
                protected
                        
                        {* Hardware Flags and Capatibilities *}
                        FIsEmulated : Boolean;
                        FRunsOnHardware : Boolean;
                        FMultithreaded : Boolean;
                        FMinGLVersionMinor : Integer;
                        FMinGLVersionMajor : Integer;
                        FActive : Boolean;                                               
                        FOption : TelGraphicsProgramOptions;
                        
                        {* The Setter *}
                        procedure setIsEmulated( val : Boolean );
                        procedure setRunsOnHardware( val : Boolean );
                        procedure setMultithreaded( val : Boolean );
                        procedure setMinGLVersionMinor( val : Integer );
                        procedure setMinGLVersionMajor( val : Integer );
                        procedure setActive( val : Boolean );                                    
                                        
                public
                        
                        {* Option Management *}
                        procedure setOptions( opts : TelGraphicsProgramOptions );virtual;
                        function getOptions : TelGraphicsProgramOptions;
                        
                        {* Ressource Handling *}
                        function createFromFile( ARessource : String ): Boolean; virtual;
                        function createFromString( ARessource : String ): Boolean; virtual;
                        function createFromMemory( AMemory : PByte ): Boolean; virtual;
                        function bindProgram : Boolean; virtual;
                        function unbindProgram : Boolean; virtual;
                        function renderProgram( ARessource : PelGraphicsRessource ): boolean; virtual;
                        
                        {* Ressources *}
                        procedure setDefaultRessource( ARessource : PelGraphicsRessource ): Boolean; virtual;
                        function getDefaultRessource : PelGraphicsRessource;
                        
                        {* Getter *}
                        function getIsEmulated : Boolean;
                        function getRunsOnHardware : Boolean;
                        function getMinGLVersionMinor : Integer;
                        function getMinGLVersionMajor : Integer;
                        function getActive : Boolean;
                
        end;
        
        
        {* The GraphicsContext creates, initializes and destroys a direct api context. It's also only a base class *}
        TelGraphicsContext = class
                protected
                
                        FCurrent : Boolean;
                        FLocked : Boolean;
                
                public
                
                        function initialize : Boolean;virtual;
                        function initialize( fFormat : TelGraphicsFormat ): Boolean;virtual;
                        
                        function shutdown : Boolean; virtual;
                        
                        procedure reset; virtual;
                        procedure update; virtual;
                
                        procedure lock;
                        procedure unlock;
                
        end;
        
        PelGraphicsContext = ^TelGraphicsContext;
        
        
        {* The TelGraphicsDevice is the core of the hole graphics system. The class is a direct layer to 
           the lowlevel graphics api provided by the os. The device has a lot of functionality and it's also
           used for every rendering operation in the hole graphics system. 
           
           But also this class is only a base class and comes with no functionality, but it gives you the power of
           platform-indiependant rendering and easy porting to other platforms. *}
        TelGraphicsDevice = class
                protected  
                        
                        {* Rendering Properties *}
                        FDoClearStencilBuffer : Boolean;
                        FDoClearDepthBuffer : Boolean;
                        FDoClearColorBuffer : Boolean;
                        FClearColor : TelColor;
                        FClearStencil : Integer;
                        FClearDepth : Integer;
                        
                        FSyncedFramerate : Integer;
                        FHasSyncedFramerate : Boolean;
                        
                        {* Context *}
                        FCurrentContext : PelGraphicsContext;
                        FHasContext : Boolean;
                        
                        {* Format Properties *}
                        FDeviceIdentifier : String;
                        FPlatformGLRequirements : String;
                        FDevicePlatforms : Array of String;
                        FDeviceHasGLRequirements : Boolean;
                        FMinGLVersionMinor : Integer;
                        FMinGLVersionMajor : Integer;
                        
                        FAutoSwapBuffer : Boolean;
                        FAutoFillBackground : Boolean;
                        FUseDoubleBuffering : Boolean;
                        FUseAdvancedRendering : Boolean;
                        FUseStereoRendering : Boolean;
                        FUseOffscreenRendering : Boolean;        
                        
                        FConfigurationFile : String;
                        
                        FHasMinResolution : Boolean;
                        FHasMaxResolution : Boolean;
                        FMinResolution : TelVector2i;
                        FMaxResolution : TelVector2i;
                        
                        
                        {* Screen Properties *}
                        FScreenResolution : TelVector2i;
                        FMountedToScreen : Boolean;
                        FMountedToOffscreen : Boolean;
                        FCurrentContext : Boolean;
                        
                        
                        {* Setter *}
                        procedure setDeviceIdentifier( dev : String );
                        procedure setPlatformGLRequirements( dev : String );
                        procedure addDevicePlattforms( dev : String );
                        procedure setDeviceHasGLRequirements( a : Boolean );
                        procedure setMinGLVersionMinor( a : Integer );
                        procedure setMinGLVersionMajor( a : Integer );
                        procedure setAutoSwapBuffer( a : Boolean );
                        procedure setAutoFillBackground( a : Boolean );
                        procedure setUseDoubleBuffering( a : Boolean );
                        procedure setUseAdvancedRendering( a : Boolean );
                        procedure setUseStereoRendering( a : Boolean );
                        procedure setUseOffscreenRendering( a : Boolean );
                        procedure setConfigurationFile( a : String );
                        procedure setHasMinResolution( a : Boolean );
                        procedure setHasMaxResolution( a : Boolean );
                        procedure setMinResolution( a : TelVector2i );
                        procedure setMaxResolution( a : TelVector2i );
     
                        
                public
                
                        {* Context Management *}
                        procedure makeCurrent( AContext : PelGraphicsContext );
                        function getCurrent : PelGraphicsContext;
                        
                        function hasContext : Boolean;
                
                        {* Simple Geometry Rendering *}
                        procedure drawLine( AWidth : Single; AFrom : TelVector2i; ATo : TelVector2i );virtual;
                        procedure drawRay( AWidth : Single; Origin : TelVector2i; Direction : TelVector2i );virtual;
                        procedure drawBox( ABox : TRect );virtual;
                        procedure drawEllipse( ARadius :Single; AOrigin : TelVector2i ); virtual;
                        
                        
                        {* RenderTarget Management *}
                        procedure setActiveRenderTarget( ATarget : TelRenderTarget );
                        procedure setActiveRenderTargetChannel( AChannel : Integer );
                        procedure setActiveRenderTargetStencilChannel( AChannel : Integer );
                        
                        function getActiveRenderTarget : TelRenderTarget;
                        function getActiveRenderTargetChannel : Integer;
                        function getActiveRenderTargetStencilChannel : Integer;
                        
                        function usesDefaultRenderTarget : Boolean; //Render To Screen?
                        procedure setDefaultRenderTarget;
                        
                
                        {* General Pixel-Based Operations *}
                        procedure setStencilRect( fRect : TRect );
                        procedure setStencilPolygon( P01 : TelVector2f; P02 : TelVector2f; P03 : TelVector2f );
                        procedure addStencilRect( fRect : TRect );
                        procedure addStencilPolygon(  P01 : TelVector2f; P02 : TelVector2f; P03 : TelVector2f );
                        procedure clearStencilOperations;
                        
                        function grabFrameBuffer : TelTexture;
                        function saveFrameBuffer( APath : String ): Boolean;
                        
                        
                        {* Buffer Clear *}
                        procedure swapCurrentBuffer; virtual;
                        procedure swapScreenBuffer; virtual;
                        
                        procedure clearScreen;virtual;
                        procedure clearColor( R,G,B : Byte );virtual;
                        procedure clearColor( R,G,B : Single );virtual;
                        procedure clearStencil( Stencil : Integer );virtual;
                        procedure clearDepth( Depth : Integer );virtual;
                        
                        procedure setClearColor( R,G,B : Byte );
                        procedure setStencilValue( Stencil : Integer );
                        procedure setDepthValue( Depth : Integer );
                        
                        function getClearColor : TelColor;
                        function getStencilValue : Integer;
                        function getDepthValue : Integer;
                        
                        {* Format Management *}
                        procedure setFormat( format : TelGraphicsFormat );
                        function getFormat(): TelGraphicsFormat;
                        function validFormat( a : TelGraphicsFormat ): Boolean; virtual;
                        
                        
                        {* Getter *}
                        function getDeviceIdentifier() : String;
                        function getPlatformGLRequirements() : String;
                        function getDevicePlatform( index : Integer ): String;
                        function getHasGLRequirements(): Boolean;
                        function getMinGLVersionMinor() : Integer;
                        function getMinGLVersionMajor() : Integer;
                        function getAutoSwapBuffer() : Boolean;
                        function getAutoFillBackground(): Boolean;
                        function getUseDoubleBuffering(): Boolean;
                        function getUseAdvancedRendering(): Boolean;
                        function getUseStereoRendering() : Boolean;
                        function getOffscreenRendering(): Boolean;
                        function getConfigurationFile(): String;
                        function getHasMinResolution(): Boolean;
                        function getHasMaxResolution(): Boolean;
                        function getMinResolution(): TelVector2i;
                        function getMaxResolution(): TelVector2i;
                        
                        function getScreenResolution(): TelVector2i;
                        function getMountedToScreen(): Boolean;
                        function getMountedToOffscreen(): Boolean;
                        function getCurrentContext(): Boolean;
                
                
        end;




end;




