Elysion Framework
=================

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

Also thanks to these pieces of software which inspired me:  
- Irrlicht 3D Engine  
- Horde 3D / Horde 3D GameEngine  
- Gosu  
- Unity3D  

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

	
That of course is just a small example of what Elysion can do. It is a full-fledged game framework and aims to be so: It allows you to play sound and music via ElysionAudio (which is based upon SDL_mixer), a texture manager which takes care of all your textures without you needing to worry about it, Elysion has a storage system similar to HTML5 Localstorage, integrated support for Cascading Stylesheets (CSS), as well as game-specific class and helpers (such as adding nodes to scenes).


Getting Started
---------------

For detailed information please visit the Elysion wiki: https://github.com/freezedev/elysion/wiki


License
-------

Elysion is dual-licensed under the MIT and MPL license. Choose which one
you like the most or use both if you feel like it.  
You are allowed to use in any project of any kind, be it open-source, commercial, closed-source or a combination those.  
You don't even need to acknowledge that you used Elysion in your application, but it is greatly appreciated.  

Hint: Other third-party libraries included in this package (in /lib/Dependencies) may use a different license.  
