Elysion Game Framework
======================


What is this?
-------------

Elysion is a cross-plaform (currently supporting Windows, Linux and Mac OS X)
framework for creating games with ease and is therefore suited for rapid prototyping.

Who made this?
-------------

Elysion has been more or less regurarly in development since 2005 and has been
created by Johannes Stein.

Thanks to anyone who used (or whom I forced to use ;) ) this framework. This  
framework would not exist in its current form without those people:  
- Sven Rech (Initial Linux testing)  
- David Herzog (Early sprite lists and in general a very helpful guy)  
- Jesse Klugmann  
- Christian Fuchs  
- Arthur Baude  
- Dominique Louis (He made SDL bindings for Object Pascal)  
- Sam Lantinga (The creator of SDL)  
And of course thanks to everyone else who used this framework or gave it a shot.

Also thanks to these pieces of software which inspired me:  
- [Irrlicht 3D Engine](http://irrlicht.sourceforge.net)  
- [Horde 3D](http://horde3d.org) / Horde 3D GameEngine  
- [Gosu](http://libgosu.org)  
- [Unity3D](http://unity3d.com)  
- [Flashpunk](http://flashpunk.net/)  
- [Sparrow Framework](http://www.sparrow-framework.org/)

What does it look like?
-----------------------

Elysion is designed to be high-level with its API to be very simple and intuitive.  
Here is a chunk of code which loads a sprite, displays the sprite and randomly  
changes its color with each click.  


	var mySprite: TelSprite;

	mySprite := TelSprite.Create;
	mySprite.LoadFromFile('mySprite.png');

	mySprite.Draw;

	if (mySprite.Click) then
	begin
	  Randomize;
	  mySprite.Color.R := Random(255);
	  mySprite.Color.G := Random(255);
	  mySprite.Color.B := Random(255);
	  // Alternative you could also write: mySprite.Color := makeCol(Random(255), Random(255), Random(255));
	end;

	
That of course is just a small example of what Elysion can do. It is a full-fledged game framework and aims to be so: It allows you to play sound and music via ElysionAudio (which is based upon SDL_mixer), a texture manager which takes care of all your textures without you needing to worry about it, Elysion has a storage system similar to HTML5 Localstorage, integrated support for Cascading Stylesheets (CSS), as well as game-specific classes and helpers (such as scene management or adding nodes to scenes).

Features
--------

Oh, you want the old plain feature list? Here you go:  

2D Drawing API:  
- Text drawing  
- Sprite management  
- Rotating, scaling and color any object  
- Blend modes

3D Drawing API:  
- Basic support for 3D through OpenGL  
- Allows integration of Horde 3D (https://github.com/Stoney-FD/horde3d-pascal)  

Audio:  
- Audio support through SDL_mixer  
- Play OGG and WAV files

Advanced features:  
- RTTI support  
- Method chaining  
- CSS support  
- Storage system (HTML5 LocalStorage inspired)


Getting Started
---------------

For detailed information please visit the Elysion wiki: https://github.com/freezedev/elysion/wiki


Need some help?
---------------

If you have some questions or hit road block during development, check out the official forums: http://www.pascalgamedevelopment.com/forumdisplay.php?67-Elysion-Game-Framework

Why greek mythology?
--------------------

I (Johannes Stein) have always had a thing for greek mythology. I called this Elysion as Elysion in greek mythology is the place for fallen loved heroes or those who have been granted immortality. At the time when I started the engine, 3D was the big thing everyone wanted to do and 2D was considered something outdated. While I don't believed in that and still don't, I think having Elysion as a name for a 2D engine is fitting name.

License
-------

Elysion is dual-licensed under the MIT and MPL license. Choose which one
you like the most or use both if you feel like it.  
You are allowed to use Elysion in any project of any kind, be it open-source, commercial, closed-source or a combination those.
You don't even need to acknowledge that you used Elysion in your application, but it is greatly appreciated if you do.

Hint: Other third-party libraries included in this package (in /lib/Dependencies) may use a different license.  
