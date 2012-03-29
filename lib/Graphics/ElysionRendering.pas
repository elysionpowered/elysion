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





