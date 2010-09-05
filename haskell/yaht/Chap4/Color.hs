data Color 
	= Red
	| Orange 
	| Yellow 
	| Green 
	| Blue
	| Purple 
	| White 
	| Black
	| Custom Int Int Int

colorToRGB Red		=	(255, 0, 0)
colorToRGB Orange	=	(255, 128, 0)
colorToRGB Yellow	=	(255, 255, 0)
colorToRGB Green	=	(0, 255, 0)
colorToRGB Blue		=	(0, 0, 255)
colorToRGB Purple	=	(255, 0, 255)
colorToRGB White	=	(255, 255, 255)
colorToRGB Black	=	(0, 0, 0)
colorToRGB (Custom r g b) = (r, g, b)

customColor = Custom 1 2 3
