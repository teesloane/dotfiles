bplist00�XUTI-Data�	
_public.utf16-plain-text_public.utf8-plain-text[public.html_$com.apple.traditional-mac-plain-textO ټ; ; ;   z e n b u r n - t h e m e . e l   - - -   A   l o w   c o n t r a s t   c o l o r   t h e m e   f o r   E m a c s .   ; ;   C o p y r i g h t   ( C )   2 0 1 1 - 2 0 1 8   B o z h i d a r   B a t s o v   ; ;   A u t h o r :   B o z h i d a r   B a t s o v   < b o z h i d a r @ b a t s o v . c o m >  ; ;   U R L :   h t t p : / / g i t h u b . c o m / b b a t s o v / z e n b u r n - e m a c s  ; ;   V e r s i o n :   2 . 7 - s n a p s h o t   ; ;   T h i s   p r o g r a m   i s   f r e e   s o f t w a r e ;   y o u   c a n   r e d i s t r i b u t e   i t   a n d / o r   m o d i f y  ; ;   i t   u n d e r   t h e   t e r m s   o f   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e   a s   p u b l i s h e d   b y  ; ;   t h e   F r e e   S o f t w a r e   F o u n d a t i o n ,   e i t h e r   v e r s i o n   3   o f   t h e   L i c e n s e ,   o r  ; ;   ( a t   y o u r   o p t i o n )   a n y   l a t e r   v e r s i o n .   ; ;   T h i s   p r o g r a m   i s   d i s t r i b u t e d   i n   t h e   h o p e   t h a t   i t   w i l l   b e   u s e f u l ,  ; ;   b u t   W I T H O U T   A N Y   W A R R A N T Y ;   w i t h o u t   e v e n   t h e   i m p l i e d   w a r r a n t y   o f  ; ;   M E R C H A N T A B I L I T Y   o r   F I T N E S S   F O R   A   P A R T I C U L A R   P U R P O S E .     S e e   t h e  ; ;   G N U   G e n e r a l   P u b l i c   L i c e n s e   f o r   m o r e   d e t a i l s .   ; ;   Y o u   s h o u l d   h a v e   r e c e i v e d   a   c o p y   o f   t h e   G N U   G e n e r a l   P u b l i c   L i c e n s e  ; ;   a l o n g   w i t h   t h i s   p r o g r a m .     I f   n o t ,   s e e   < h t t p : / / w w w . g n u . o r g / l i c e n s e s / > .   ; ; ;   C o m m e n t a r y :   ; ;   A   p o r t   o f   t h e   p o p u l a r   V i m   t h e m e   Z e n b u r n   f o r   E m a c s   2 4 + ,   b u i l t   o n   t o p  ; ;   o f   t h e   n e w   b u i l t - i n   t h e m e   s u p p o r t   i n   E m a c s   2 4 .   ; ; ;   C r e d i t s :   ; ;   J a n i   N u r m i n e n   c r e a t e d   t h e   o r i g i n a l   t h e m e   f o r   v i m   o n   w h i c h   t h i s   p o r t  ; ;   i s   b a s e d .   ; ; ;   C o d e :   ( d e f t h e m e   z e n b u r n   " T h e   Z e n b u r n   c o l o r   t h e m e " )   ( d e f g r o u p   z e n b u r n - t h e m e   n i l      " Z e n b u r n   t h e m e . "      : p r e f i x   " z e n b u r n - t h e m e - "      : l i n k   ' ( u r l - l i n k   : t a g   " G i t H u b "   " h t t p : / / g i t h u b . c o m / b b a t s o v / z e n b u r n - e m a c s " )      : t a g   " Z e n b u r n   t h e m e " )   ; ; ; # # # a u t o l o a d  ( d e f c u s t o m   z e n b u r n - o v e r r i d e - c o l o r s - a l i s t   ' ( )      " P l a c e   t o   o v e r r i d e   d e f a u l t   t h e m e   c o l o r s .   Y o u   c a n   o v e r r i d e   a   s u b s e t   o f   t h e   t h e m e ' s   d e f a u l t   c o l o r s   b y  d e f i n i n g   t h e m   i n   t h i s   a l i s t . "      : g r o u p   ' z e n b u r n - t h e m e      : t y p e   ' ( a l i s t                      : k e y - t y p e   ( s t r i n g   : t a g   " N a m e " )                      : v a l u e - t y p e   ( s t r i n g   : t a g   "   H e x " ) ) )   ( d e f c u s t o m   z e n b u r n - u s e - v a r i a b l e - p i t c h   n i l      " U s e   v a r i a b l e   p i t c h   f a c e   f o r   s o m e   h e a d i n g s   a n d   t i t l e s . "      : t y p e   ' b o o l e a n      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - h e i g h t - m i n u s - 1   0 . 8      " F o n t   s i z e   - 1 . "      : t y p e   ' n u m b e r      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - h e i g h t - p l u s - 1   1 . 1      " F o n t   s i z e   + 1 . "      : t y p e   ' n u m b e r      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - h e i g h t - p l u s - 2   1 . 1 5      " F o n t   s i z e   + 2 . "      : t y p e   ' n u m b e r      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - h e i g h t - p l u s - 3   1 . 2      " F o n t   s i z e   + 3 . "      : t y p e   ' n u m b e r      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - h e i g h t - p l u s - 4   1 . 3      " F o n t   s i z e   + 4 . "      : t y p e   ' n u m b e r      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - s c a l e - o r g - h e a d l i n e s   n i l      " W h e t h e r   ` o r g - m o d e '   h e a d l i n e s   s h o u l d   b e   s c a l e d . "      : t y p e   ' b o o l e a n      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ( d e f c u s t o m   z e n b u r n - s c a l e - o u t l i n e - h e a d l i n e s   n i l      " W h e t h e r   ` o u t l i n e - m o d e '   h e a d l i n e s   s h o u l d   b e   s c a l e d . "      : t y p e   ' b o o l e a n      : g r o u p   ' z e n b u r n - t h e m e      : p a c k a g e - v e r s i o n   ' ( z e n b u r n   .   " 2 . 6 " ) )   ; ; ;   C o l o r   P a l e t t e   ( d e f v a r   z e n b u r n - d e f a u l t - c o l o r s - a l i s t      ' ( ( " z e n b u r n - f g + 1 "           .   " # F F F F E F " )          ( " z e n b u r n - f g "               .   " # D C D C C C " )          ( " z e n b u r n - f g - 1 "           .   " # 6 5 6 5 5 5 " )          ( " z e n b u r n - b g - 2 "           .   " # 0 0 0 0 0 0 " )          ( " z e n b u r n - b g - 1 "           .   " # 2 B 2 B 2 B " )          ( " z e n b u r n - b g - 0 5 "         .   " # 3 8 3 8 3 8 " )          ( " z e n b u r n - b g "               .   " # 3 F 3 F 3 F " )          ( " z e n b u r n - b g + 0 5 "         .   " # 4 9 4 9 4 9 " )          ( " z e n b u r n - b g + 1 "           .   " # 4 F 4 F 4 F " )          ( " z e n b u r n - b g + 2 "           .   " # 5 F 5 F 5 F " )          ( " z e n b u r n - b g + 3 "           .   " # 6 F 6 F 6 F " )          ( " z e n b u r n - r e d + 2 "         .   " # E C B 3 B 3 " )          ( " z e n b u r n - r e d + 1 "         .   " # D C A 3 A 3 " )          ( " z e n b u r n - r e d "             .   " # C C 9 3 9 3 " )          ( " z e n b u r n - r e d - 1 "         .   " # B C 8 3 8 3 " )          ( " z e n b u r n - r e d - 2 "         .   " # A C 7 3 7 3 " )          ( " z e n b u r n - r e d - 3 "         .   " # 9 C 6 3 6 3 " )          ( " z e n b u r n - r e d - 4 "         .   " # 8 C 5 3 5 3 " )          ( " z e n b u r n - r e d - 5 "         .   " # 7 C 4 3 4 3 " )          ( " z e n b u r n - r e d - 6 "         .   " # 6 C 3 3 3 3 " )          ( " z e n b u r n - o r a n g e "       .   " # D F A F 8 F " )          ( " z e n b u r n - y e l l o w "       .   " # F 0 D F A F " )          ( " z e n b u r n - y e l l o w - 1 "   .   " # E 0 C F 9 F " )          ( " z e n b u r n - y e l l o w - 2 "   .   " # D 0 B F 8 F " )          ( " z e n b u r n - g r e e n - 5 "     .   " # 2 F 4 F 2 F " )          ( " z e n b u r n - g r e e n - 4 "     .   " # 3 F 5 F 3 F " )          ( " z e n b u r n - g r e e n - 3 "     .   " # 4 F 6 F 4 F " )          ( " z e n b u r n - g r e e n - 2 "     .   " # 5 F 7 F 5 F " )          ( " z e n b u r n - g r e e n - 1 "     .   " # 6 F 8 F 6 F " )          ( " z e n b u r n - g r e e n "         .   " # 7 F 9 F 7 F " )          ( " z e n b u r n - g r e e n + 1 "     .   " # 8 F B 2 8 F " )          ( " z e n b u r n - g r e e n + 2 "     .   " # 9 F C 5 9 F " )          ( " z e n b u r n - g r e e n + 3 "     .   " # A F D 8 A F " )          ( " z e n b u r n - g r e e n + 4 "     .   " # B F E B B F " )          ( " z e n b u r n - c y a n "           .   " # 9 3 E 0 E 3 " )          ( " z e n b u r n - b l u e + 3 "       .   " # B D E 0 F 3 " )          ( " z e n b u r n - b l u e + 2 "       .   " # A C E 0 E 3 " )          ( " z e n b u r n - b l u e + 1 "       .   " # 9 4 B F F 3 " )          ( " z e n b u r n - b l u e "           .   " # 8 C D 0 D 3 " )          ( " z e n b u r n - b l u e - 1 "       .   " # 7 C B 8 B B " )          ( " z e n b u r n - b l u e - 2 "       .   " # 6 C A 0 A 3 " )          ( " z e n b u r n - b l u e - 3 "       .   " # 5 C 8 8 8 B " )          ( " z e n b u r n - b l u e - 4 "       .   " # 4 C 7 0 7 3 " )          ( " z e n b u r n - b l u e - 5 "       .   " # 3 6 6 0 6 0 " )          ( " z e n b u r n - m a g e n t a "     .   " # D C 8 C C 3 " ) )      " L i s t   o f   Z e n b u r n   c o l o r s .  E a c h   e l e m e n t   h a s   t h e   f o r m   ( N A M E   .   H E X ) .   ` + N '   s u f f i x e s   i n d i c a t e   a   c o l o r   i s   l i g h t e r .  ` - N '   s u f f i x e s   i n d i c a t e   a   c o l o r   i s   d a r k e r . " )   ( d e f m a c r o   z e n b u r n - w i t h - c o l o r - v a r i a b l e s   ( & r e s t   b o d y )      " ` l e t '   b i n d   a l l   c o l o r s   d e f i n e d   i n   ` z e n b u r n - c o l o r s - a l i s t '   a r o u n d   B O D Y .  A l s o   b i n d   ` c l a s s '   t o   ( ( c l a s s   c o l o r )   ( m i n - c o l o r s   8 9 ) ) . "      ( d e c l a r e   ( i n d e n t   0 ) )      ` ( l e t   ( ( c l a s s   ' ( ( c l a s s   c o l o r )   ( m i n - c o l o r s   8 9 ) ) )                    , @ ( m a p c a r   ( l a m b d a   ( c o n s )                                            ( l i s t   ( i n t e r n   ( c a r   c o n s ) )   ( c d r   c o n s ) ) )                                        ( a p p e n d   z e n b u r n - d e f a u l t - c o l o r s - a l i s t                                                        z e n b u r n - o v e r r i d e - c o l o r s - a l i s t ) )                    ( z - v a r i a b l e - p i t c h   ( i f   z e n b u r n - u s e - v a r i a b l e - p i t c h                                                                ' v a r i a b l e - p i t c h   ' d e f a u l t ) ) )            , @ b o d y ) )   ; ; ;   T h e m e   F a c e s  ( z e n b u r n - w i t h - c o l o r - v a r i a b l e s      ( c u s t o m - t h e m e - s e t - f a c e s        ' z e n b u r n  ; ; ; ;   B u i l t - i n  ; ; ; ; ;   b a s i c   c o l o r i n g        ' ( b u t t o n   ( ( t   ( : u n d e r l i n e   t ) ) ) )        ` ( l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : u n d e r l i n e   t   : w e i g h t   b o l d ) ) ) )        ` ( l i n k - v i s i t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : u n d e r l i n e   t   : w e i g h t   n o r m a l ) ) ) )        ` ( d e f a u l t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( c u r s o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - f g + 1 ) ) ) )        ` ( w i d g e t - f i e l d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( e s c a p e - g l y p h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( f r i n g e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( h e a d e r - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w                                                                      : b a c k g r o u n d   , z e n b u r n - b g - 1                                                                      : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   r e l e a s e d - b u t t o n ) ) ) ) )        ` ( h i g h l i g h t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) ) ) )        ` ( s u c c e s s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   b o l d ) ) ) )        ` ( w a r n i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( t o o l t i p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )  ; ; ; ; ;   c o m p i l a t i o n        ` ( c o m p i l a t i o n - c o l u m n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( c o m p i l a t i o n - e n t e r - d i r e c t o r y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( c o m p i l a t i o n - e r r o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( c o m p i l a t i o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( c o m p i l a t i o n - i n f o - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( c o m p i l a t i o n - i n f o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4   : u n d e r l i n e   t ) ) ) )        ` ( c o m p i l a t i o n - l e a v e - d i r e c t o r y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( c o m p i l a t i o n - l i n e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( c o m p i l a t i o n - l i n e - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( c o m p i l a t i o n - m e s s a g e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( c o m p i l a t i o n - w a r n i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( c o m p i l a t i o n - m o d e - l i n e - e x i t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : w e i g h t   b o l d ) ) ) )        ` ( c o m p i l a t i o n - m o d e - l i n e - f a i l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( c o m p i l a t i o n - m o d e - l i n e - r u n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   c o m p l e t i o n s        ` ( c o m p l e t i o n s - a n n o t a t i o n s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )  ; ; ; ; ;   e w w        ' ( e w w - i n v a l i d - c e r t i f i c a t e   ( ( t   ( : i n h e r i t   e r r o r ) ) ) )        ' ( e w w - v a l i d - c e r t i f i c a t e       ( ( t   ( : i n h e r i t   s u c c e s s ) ) ) )  ; ; ; ; ;   g r e p        ` ( g r e p - c o n t e x t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( g r e p - e r r o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( g r e p - h i t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g r e p - m a t c h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( m a t c h   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   h i - l o c k        ` ( h i - b l u e         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - c y a n         : f o r e g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( h i - g r e e n       ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n + 4   : f o r e g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( h i - p i n k         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - m a g e n t a   : f o r e g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( h i - y e l l o w     ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w     : f o r e g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( h i - b l u e - b     ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e         : w e i g h t           b o l d ) ) ) )        ` ( h i - g r e e n - b   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : w e i g h t           b o l d ) ) ) )        ` ( h i - r e d - b       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d           : w e i g h t           b o l d ) ) ) )  ; ; ; ; ;   i n f o        ` ( I n f o - q u o t e d   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )  ; ; ; ; ;   i s e a r c h        ` ( i s e a r c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : w e i g h t   b o l d   : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( i s e a r c h - f a i l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( l a z y - h i g h l i g h t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : w e i g h t   b o l d   : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) ) ) )         ` ( m e n u   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( m i n i b u f f e r - p r o m p t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m o d e - l i n e            ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1                                                        : b a c k g r o u n d   , z e n b u r n - b g - 1                                                        : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   r e l e a s e d - b u t t o n ) ) )              ( t   : i n v e r s e - v i d e o   t ) ) )        ` ( m o d e - l i n e - b u f f e r - i d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m o d e - l i n e - i n a c t i v e            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2                                              : b a c k g r o u n d   , z e n b u r n - b g - 0 5                                              : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   r e l e a s e d - b u t t o n ) ) ) ) )        ` ( r e g i o n   ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) )                            ( t   : i n v e r s e - v i d e o   t ) ) )        ` ( s e c o n d a r y - s e l e c t i o n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( t r a i l i n g - w h i t e s p a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( v e r t i c a l - b o r d e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )  ; ; ; ; ;   f o n t   l o c k        ` ( f o n t - l o c k - b u i l t i n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( f o n t - l o c k - c o m m e n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( f o n t - l o c k - c o m m e n t - d e l i m i t e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( f o n t - l o c k - c o n s t a n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( f o n t - l o c k - d o c - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( f o n t - l o c k - f u n c t i o n - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( f o n t - l o c k - k e y w o r d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( f o n t - l o c k - n e g a t i o n - c h a r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( f o n t - l o c k - p r e p r o c e s s o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( f o n t - l o c k - r e g e x p - g r o u p i n g - c o n s t r u c t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( f o n t - l o c k - r e g e x p - g r o u p i n g - b a c k s l a s h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   b o l d ) ) ) )        ` ( f o n t - l o c k - s t r i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( f o n t - l o c k - t y p e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( f o n t - l o c k - v a r i a b l e - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( f o n t - l o c k - w a r n i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : w e i g h t   b o l d ) ) ) )         ` ( c - a n n o t a t i o n - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )  ; ; ; ; ;   l i n e   n u m b e r s   ( E m a c s   2 6 . 1   a n d   a b o v e )        ` ( l i n e - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3   : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) ) ) )        ` ( l i n e - n u m b e r - c u r r e n t - l i n e   ( ( t   ( : i n h e r i t   l i n e - n u m b e r   : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )  ; ; ; ; ;   m a n        ' ( M a n - o v e r s t r i k e   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ' ( M a n - u n d e r l i n e     ( ( t   ( : i n h e r i t   ( f o n t - l o c k - s t r i n g - f a c e   u n d e r l i n e ) ) ) ) )  ; ; ; ; ;   n e w s t i c k e r        ` ( n e w s t i c k e r - d a t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - d e f a u l t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - e n c l o s u r e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( n e w s t i c k e r - e x t r a - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 2   : h e i g h t   0 . 8 ) ) ) )        ` ( n e w s t i c k e r - f e e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - i m m o r t a l - i t e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( n e w s t i c k e r - n e w - i t e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( n e w s t i c k e r - o b s o l e t e - i t e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( n e w s t i c k e r - o l d - i t e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( n e w s t i c k e r - s t a t i s t i c s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - i m m o r t a l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - l i s t w i n d o w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - n e w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - o b s o l e t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - o l d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( n e w s t i c k e r - t r e e v i e w - s e l e c t i o n - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   w o m a n        ' ( w o m a n - b o l d       ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ' ( w o m a n - i t a l i c   ( ( t   ( : i n h e r i t   ( f o n t - l o c k - s t r i n g - f a c e   i t a l i c ) ) ) ) )  ; ; ; ;   T h i r d - p a r t y  ; ; ; ; ;   a c e - j u m p        ` ( a c e - j u m p - f a c e - b a c k g r o u n d            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l ) ) ) )        ` ( a c e - j u m p - f a c e - f o r e g r o u n d            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l ) ) ) )  ; ; ; ; ;   a c e - w i n d o w        ` ( a w - b a c k g r o u n d - f a c e            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l ) ) ) )        ` ( a w - l e a d i n g - c h a r - f a c e   ( ( t   ( : i n h e r i t   a w - m o d e - l i n e - f a c e ) ) ) )  ; ; ; ; ;   a n d r o i d   m o d e        ` ( a n d r o i d - m o d e - d e b u g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( a n d r o i d - m o d e - e r r o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( a n d r o i d - m o d e - i n f o - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( a n d r o i d - m o d e - v e r b o s e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( a n d r o i d - m o d e - w a r n i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   a n z u        ` ( a n z u - m o d e - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )        ` ( a n z u - m o d e - l i n e - n o - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( a n z u - m a t c h - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( a n z u - m a t c h - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( a n z u - m a t c h - 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( a n z u - r e p l a c e - t o   ( ( t   ( : i n h e r i t   a n z u - r e p l a c e - h i g h l i g h t   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   a u c t e x        ` ( f o n t - l a t e x - b o l d - f a c e   ( ( t   ( : i n h e r i t   b o l d ) ) ) )        ` ( f o n t - l a t e x - w a r n i n g - f a c e   ( ( t   ( : f o r e g r o u n d   n i l   : i n h e r i t   f o n t - l o c k - w a r n i n g - f a c e ) ) ) )        ` ( f o n t - l a t e x - s e c t i o n i n g - 5 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d   ) ) ) )        ` ( f o n t - l a t e x - s e d a t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( f o n t - l a t e x - i t a l i c - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : s l a n t   i t a l i c ) ) ) )        ` ( f o n t - l a t e x - s t r i n g - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - s t r i n g - f a c e ) ) ) )        ` ( f o n t - l a t e x - m a t h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( f o n t - l a t e x - s c r i p t - c h a r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )  ; ; ; ; ;   a g d a - m o d e        ` ( a g d a 2 - h i g h l i g h t - k e y w o r d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - s t r i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - s y m b o l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - p r i m i t i v e - t y p e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - i n d u c t i v e - c o n s t r u c t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - c o i n d u c t i v e - c o n s t r u c t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - d a t a t y p e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - f u n c t i o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - m o d u l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - e r r o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - u n s o l v e d - m e t a - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - u n s o l v e d - c o n s t r a i n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - t e r m i n a t i o n - p r o b l e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - i n c o m p l e t e - p a t t e r n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( a g d a 2 - h i g h l i g h t - t y p e c h e c k s - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )  ; ; ; ; ;   a u t o - c o m p l e t e        ` ( a c - c a n d i d a t e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 3   : f o r e g r o u n d   , z e n b u r n - b g - 2 ) ) ) )        ` ( a c - s e l e c t i o n - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 4   : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( p o p u p - t i p - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w - 2   : f o r e g r o u n d   , z e n b u r n - b g - 2 ) ) ) )        ` ( p o p u p - m e n u - m o u s e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w - 2   : f o r e g r o u n d   , z e n b u r n - b g - 2 ) ) ) )        ` ( p o p u p - s u m m a r y - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 3   : f o r e g r o u n d   , z e n b u r n - b g - 2 ) ) ) )        ` ( p o p u p - s c r o l l - b a r - f o r e g r o u n d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( p o p u p - s c r o l l - b a r - b a c k g r o u n d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( p o p u p - i s e a r c h - m a t c h   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g   : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )  ; ; ; ; ;   a v y        ` ( a v y - b a c k g r o u n d - f a c e            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l ) ) ) )        ` ( a v y - l e a d - f a c e - 0            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l   : w e i g h t   b o l d ) ) ) )        ` ( a v y - l e a d - f a c e - 1            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l   : w e i g h t   b o l d ) ) ) )        ` ( a v y - l e a d - f a c e - 2            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l   : w e i g h t   b o l d ) ) ) )        ` ( a v y - l e a d - f a c e            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g   : i n v e r s e - v i d e o   n i l   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   c o m p a n y - m o d e        ` ( c o m p a n y - t o o l t i p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( c o m p a n y - t o o l t i p - a n n o t a t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( c o m p a n y - t o o l t i p - a n n o t a t i o n - s e l e c t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( c o m p a n y - t o o l t i p - s e l e c t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( c o m p a n y - t o o l t i p - m o u s e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( c o m p a n y - t o o l t i p - c o m m o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( c o m p a n y - t o o l t i p - c o m m o n - s e l e c t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( c o m p a n y - s c r o l l b a r - f g   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( c o m p a n y - s c r o l l b a r - b g   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( c o m p a n y - p r e v i e w   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( c o m p a n y - p r e v i e w - c o m m o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )  ; ; ; ; ;   b m        ` ( b m - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w - 1   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( b m - f r i n g e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w - 1   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( b m - f r i n g e - p e r s i s t e n t - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 2   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( b m - p e r s i s t e n t - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 2   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   c a l f w        ` ( c f w : f a c e - a n n o t a t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : i n h e r i t   c f w : f a c e - d a y - t i t l e ) ) ) )        ` ( c f w : f a c e - d a y - t i t l e   ( ( t   n i l ) ) )        ` ( c f w : f a c e - d e f a u l t - c o n t e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( c f w : f a c e - d e f a u l t - d a y   ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( c f w : f a c e - d i s a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )        ` ( c f w : f a c e - g r i d   ( ( t   ( : i n h e r i t   s h a d o w ) ) ) )        ` ( c f w : f a c e - h e a d e r   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ` ( c f w : f a c e - h o l i d a y   ( ( t   ( : i n h e r i t   c f w : f a c e - s u n d a y ) ) ) )        ` ( c f w : f a c e - p e r i o d s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( c f w : f a c e - s a t u r d a y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( c f w : f a c e - s e l e c t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( c f w : f a c e - s u n d a y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( c f w : f a c e - t i t l e   ( ( t   ( : h e i g h t   2 . 0   : i n h e r i t   ( v a r i a b l e - p i t c h   f o n t - l o c k - k e y w o r d - f a c e ) ) ) ) )        ` ( c f w : f a c e - t o d a y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )        ` ( c f w : f a c e - t o d a y - t i t l e   ( ( t   ( : i n h e r i t   h i g h l i g h t   b o l d ) ) ) )        ` ( c f w : f a c e - t o o l b a r   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( c f w : f a c e - t o o l b a r - b u t t o n - o f f   ( ( t   ( : u n d e r l i n e   n i l   : i n h e r i t   l i n k ) ) ) )        ` ( c f w : f a c e - t o o l b a r - b u t t o n - o n   ( ( t   ( : u n d e r l i n e   n i l   : i n h e r i t   l i n k - v i s i t e d ) ) ) )  ; ; ; ; ;   c i d e r        ` ( c i d e r - r e s u l t - o v e r l a y - f a c e   ( ( t   ( : b a c k g r o u n d   u n s p e c i f i e d ) ) ) )        ` ( c i d e r - e n l i g h t e n e d - f a c e   ( ( t   ( : b o x   ( : c o l o r   , z e n b u r n - o r a n g e   : l i n e - w i d t h   - 1 ) ) ) ) )        ` ( c i d e r - e n l i g h t e n e d - l o c a l - f a c e   ( ( t   ( : w e i g h t   b o l d   : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( c i d e r - d e p r e c a t e d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( c i d e r - i n s t r u m e n t e d - f a c e   ( ( t   ( : b o x   ( : c o l o r   , z e n b u r n - r e d   : l i n e - w i d t h   - 1 ) ) ) ) )        ` ( c i d e r - t r a c e d - f a c e   ( ( t   ( : b o x   ( : c o l o r   , z e n b u r n - c y a n   : l i n e - w i d t h   - 1 ) ) ) ) )        ` ( c i d e r - t e s t - f a i l u r e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( c i d e r - t e s t - e r r o r - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( c i d e r - t e s t - s u c c e s s - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( c i d e r - f r i n g e - g o o d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )  ; ; ; ; ;   c i r c e        ` ( c i r c e - h i g h l i g h t - n i c k - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( c i r c e - m y - m e s s a g e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( c i r c e - f o o l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( c i r c e - t o p i c - d i f f - r e m o v e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( c i r c e - o r i g i n a t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( c i r c e - s e r v e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( c i r c e - t o p i c - d i f f - n e w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( c i r c e - p r o m p t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   c o n t e x t - c o l o r i n g        ` ( c o n t e x t - c o l o r i n g - l e v e l - 0 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - f g ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 1 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - c y a n ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 2 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 3 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 4 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 5 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 6 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 7 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 8 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) )        ` ( c o n t e x t - c o l o r i n g - l e v e l - 9 - f a c e   ( ( t   : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) )  ; ; ; ; ;   c o q        ` ( c o q - s o l v e - t a c t i c s - f a c e   ( ( t   ( : f o r e g r o u n d   n i l   : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )  ; ; ; ; ;   c t a b l e        ` ( c t b l : f a c e - c e l l - s e l e c t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( c t b l : f a c e - c o n t i n u e - b a r   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( c t b l : f a c e - r o w - s e l e c t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - c y a n   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   d e b b u g s        ` ( d e b b u g s - g n u - d o n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )        ` ( d e b b u g s - g n u - h a n d l e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( d e b b u g s - g n u - n e w   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d e b b u g s - g n u - p e n d i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( d e b b u g s - g n u - s t a l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d e b b u g s - g n u - t a g g e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   d i f f        ` ( d i f f - a d d e d                     ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 5   : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( d i f f - c h a n g e d                 ( ( t   ( : b a c k g r o u n d   " # 5 5 5 5 1 1 "   : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( d i f f - r e m o v e d                 ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 6   : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( d i f f - r e f i n e - a d d e d       ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 4   : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( d i f f - r e f i n e - c h a n g e d   ( ( t   ( : b a c k g r o u n d   " # 8 8 8 8 1 1 "   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i f f - r e f i n e - r e m o v e d   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 5   : f o r e g r o u n d   , z e n b u r n - r e d + 2 ) ) ) )        ` ( d i f f - h e a d e r   ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) )                                      ( t   ( : b a c k g r o u n d   , z e n b u r n - f g   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( d i f f - f i l e - h e a d e r            ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) )              ( t   ( : b a c k g r o u n d   , z e n b u r n - f g   : f o r e g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   d i f f - h l        ` ( d i f f - h l - c h a n g e   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : b a c k g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( d i f f - h l - d e l e t e   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : b a c k g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( d i f f - h l - i n s e r t   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1   : b a c k g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )  ; ; ; ; ;   d i m - a u t o l o a d        ` ( d i m - a u t o l o a d - c o o k i e - l i n e   ( ( t   : f o r e g r o u n d   , z e n b u r n - b g + 1 ) ) )  ; ; ; ; ;   d i r e d +        ` ( d i r e d p - d i s p l a y - m s g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( d i r e d p - c o m p r e s s e d - f i l e - s u f f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d i r e d p - d a t e - t i m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( d i r e d p - d e l e t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d p - d e l e t i o n - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d p - d i r - h e a d i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( d i r e d p - d i r - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( d i r e d p - e x e c - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d p - e x e c u t a b l e - t a g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( d i r e d p - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( d i r e d p - f i l e - s u f f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( d i r e d p - f l a g - m a r k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d p - f l a g - m a r k - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d i r e d p - i g n o r e d - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d p - l i n k - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d p - m o d e - l i n e - f l a g g e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d p - m o d e - l i n e - m a r k e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d i r e d p - n o - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( d i r e d p - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( d i r e d p - o t h e r - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( d i r e d p - r a r e - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( d i r e d p - r e a d - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( d i r e d p - s y m l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d p - w r i t e - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )  ; ; ; ; ;   d i r e d - a s y n c        ` ( d i r e d - a s y n c - f a i l u r e s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( d i r e d - a s y n c - m e s s a g e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( d i r e d - a s y n c - m o d e - m e s s a g e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   d i r e d f l        ` ( d i r e d f l - c o m p r e s s e d - f i l e - s u f f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d i r e d f l - d a t e - t i m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( d i r e d f l - d e l e t i o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d f l - d e l e t i o n - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d f l - d i r - h e a d i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( d i r e d f l - d i r - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( d i r e d f l - e x e c - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d f l - e x e c u t a b l e - t a g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( d i r e d f l - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( d i r e d f l - f i l e - s u f f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( d i r e d f l - f l a g - m a r k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d f l - f l a g - m a r k - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( d i r e d f l - i g n o r e d - f i l e - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( d i r e d f l - l i n k - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d f l - n o - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( d i r e d f l - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( d i r e d f l - o t h e r - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( d i r e d f l - r a r e - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( d i r e d f l - r e a d - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 1 ) ) ) )        ` ( d i r e d f l - s y m l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( d i r e d f l - w r i t e - p r i v   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )  ; ; ; ; ;   e d i f f        ` ( e d i f f - c u r r e n t - d i f f - A   ( ( t   ( : i n h e r i t   d i f f - r e m o v e d ) ) ) )        ` ( e d i f f - c u r r e n t - d i f f - A n c e s t o r   ( ( t   ( : i n h e r i t   e d i f f - c u r r e n t - d i f f - A ) ) ) )        ` ( e d i f f - c u r r e n t - d i f f - B   ( ( t   ( : i n h e r i t   d i f f - a d d e d ) ) ) )        ` ( e d i f f - c u r r e n t - d i f f - C   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 2   : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( e d i f f - e v e n - d i f f - A   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( e d i f f - e v e n - d i f f - A n c e s t o r   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( e d i f f - e v e n - d i f f - B   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( e d i f f - e v e n - d i f f - C   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( e d i f f - f i n e - d i f f - A   ( ( t   ( : i n h e r i t   d i f f - r e f i n e - r e m o v e d   : w e i g h t   b o l d ) ) ) )        ` ( e d i f f - f i n e - d i f f - A n c e s t o r   ( ( t   ( : i n h e r i t   e d i f f - f i n e - d i f f - A ) ) ) )        ` ( e d i f f - f i n e - d i f f - B   ( ( t   ( : i n h e r i t   d i f f - r e f i n e - a d d e d   : w e i g h t   b o l d ) ) ) )        ` ( e d i f f - f i n e - d i f f - C   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 3   : b a c k g r o u n d   , z e n b u r n - b l u e - 4   : w e i g h t   b o l d ) ) ) )        ` ( e d i f f - o d d - d i f f - A   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( e d i f f - o d d - d i f f - A n c e s t o r   ( ( t   ( : i n h e r i t   e d i f f - o d d - d i f f - A ) ) ) )        ` ( e d i f f - o d d - d i f f - B   ( ( t   ( : i n h e r i t   e d i f f - o d d - d i f f - A ) ) ) )        ` ( e d i f f - o d d - d i f f - C   ( ( t   ( : i n h e r i t   e d i f f - o d d - d i f f - A ) ) ) )  ; ; ; ; ;   e g g        ` ( e g g - t e x t - b a s e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( e g g - h e l p - h e a d e r - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e g g - h e l p - h e a d e r - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( e g g - b r a n c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e g g - b r a n c h - m o n o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e g g - t e r m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e g g - d i f f - a d d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( e g g - d i f f - d e l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( e g g - d i f f - f i l e - h e a d e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( e g g - s e c t i o n - t i t l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e g g - s t a s h - m o n o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )  ; ; ; ; ;   e l f e e d        ` ( e l f e e d - l o g - e r r o r - l e v e l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( e l f e e d - l o g - i n f o - l e v e l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( e l f e e d - l o g - w a r n - l e v e l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e l f e e d - s e a r c h - d a t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1   : u n d e r l i n e   t                                                                                              : w e i g h t   b o l d ) ) ) )        ` ( e l f e e d - s e a r c h - t a g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( e l f e e d - s e a r c h - f e e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )  ; ; ; ; ;   e m a c s - w 3 m        ` ( w 3 m - a n c h o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : u n d e r l i n e   t                                                                    : w e i g h t   b o l d ) ) ) )        ` ( w 3 m - a r r i v e d - a n c h o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2                                                                                    : u n d e r l i n e   t   : w e i g h t   n o r m a l ) ) ) )        ` ( w 3 m - f o r m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : u n d e r l i n e   t ) ) ) )        ` ( w 3 m - h e a d e r - l i n e - l o c a t i o n - t i t l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w                                                                                                            : u n d e r l i n e   t   : w e i g h t   b o l d ) ) ) )        ' ( w 3 m - h i s t o r y - c u r r e n t - u r l   ( ( t   ( : i n h e r i t   m a t c h ) ) ) )        ` ( w 3 m - l n u m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( w 3 m - l n u m - m a t c h   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1                                                                            : f o r e g r o u n d   , z e n b u r n - o r a n g e                                                                            : w e i g h t   b o l d ) ) ) )        ` ( w 3 m - l n u m - m i n i b u f f e r - p r o m p t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   e r c        ` ( e r c - a c t i o n - f a c e   ( ( t   ( : i n h e r i t   e r c - d e f a u l t - f a c e ) ) ) )        ` ( e r c - b o l d - f a c e   ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( e r c - c u r r e n t - n i c k - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( e r c - d a n g e r o u s - h o s t - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - w a r n i n g - f a c e ) ) ) )        ` ( e r c - d e f a u l t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( e r c - d i r e c t - m s g - f a c e   ( ( t   ( : i n h e r i t   e r c - d e f a u l t - f a c e ) ) ) )        ` ( e r c - e r r o r - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - w a r n i n g - f a c e ) ) ) )        ` ( e r c - f o o l - f a c e   ( ( t   ( : i n h e r i t   e r c - d e f a u l t - f a c e ) ) ) )        ` ( e r c - h i g h l i g h t - f a c e   ( ( t   ( : i n h e r i t   h o v e r - h i g h l i g h t ) ) ) )        ` ( e r c - i n p u t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( e r c - k e y w o r d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( e r c - n i c k - d e f a u l t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( e r c - m y - n i c k - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( e r c - n i c k - m s g - f a c e   ( ( t   ( : i n h e r i t   e r c - d e f a u l t - f a c e ) ) ) )        ` ( e r c - n o t i c e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( e r c - p a l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( e r c - p r o m p t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )        ` ( e r c - t i m e s t a m p - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( e r c - u n d e r l i n e - f a c e   ( ( t   ( : u n d e r l i n e   t ) ) ) )  ; ; ; ; ;   e r o s        ` ( e r o s - r e s u l t - o v e r l a y - f a c e   ( ( t   ( : b a c k g r o u n d   u n s p e c i f i e d ) ) ) )  ; ; ; ; ;   e r t        ` ( e r t - t e s t - r e s u l t - e x p e c t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( e r t - t e s t - r e s u l t - u n e x p e c t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   e s h e l l        ` ( e s h e l l - p r o m p t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( e s h e l l - l s - a r c h i v e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d ) ) ) )        ` ( e s h e l l - l s - b a c k u p   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( e s h e l l - l s - c l u t t e r   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( e s h e l l - l s - d i r e c t o r y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : w e i g h t   b o l d ) ) ) )        ` ( e s h e l l - l s - e x e c u t a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : w e i g h t   b o l d ) ) ) )        ` ( e s h e l l - l s - u n r e a d a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( e s h e l l - l s - m i s s i n g   ( ( t   ( : i n h e r i t   f o n t - l o c k - w a r n i n g - f a c e ) ) ) )        ` ( e s h e l l - l s - p r o d u c t   ( ( t   ( : i n h e r i t   f o n t - l o c k - d o c - f a c e ) ) ) )        ` ( e s h e l l - l s - s p e c i a l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( e s h e l l - l s - s y m l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   f l x        ` ( f l x - h i g h l i g h t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   f l y c h e c k        ` ( f l y c h e c k - e r r o r            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - r e d - 1 )   : i n h e r i t   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y c h e c k - w a r n i n g            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - y e l l o w )   : i n h e r i t   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y c h e c k - i n f o            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - c y a n )   : i n h e r i t   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y c h e c k - f r i n g e - e r r o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d ) ) ) )        ` ( f l y c h e c k - f r i n g e - w a r n i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( f l y c h e c k - f r i n g e - i n f o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   f l y m a k e        ` ( f l y m a k e - e r r l i n e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - r e d )                                        : i n h e r i t   u n s p e c i f i e d   : f o r e g r o u n d   u n s p e c i f i e d   : b a c k g r o u n d   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y m a k e - w a r n l i n e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - o r a n g e )                                        : i n h e r i t   u n s p e c i f i e d   : f o r e g r o u n d   u n s p e c i f i e d   : b a c k g r o u n d   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y m a k e - i n f o l i n e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - g r e e n )                                        : i n h e r i t   u n s p e c i f i e d   : f o r e g r o u n d   u n s p e c i f i e d   : b a c k g r o u n d   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )  ; ; ; ; ;   f l y s p e l l        ` ( f l y s p e l l - d u p l i c a t e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - o r a n g e )   : i n h e r i t   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( f l y s p e l l - i n c o r r e c t            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - r e d )   : i n h e r i t   u n s p e c i f i e d ) )              ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )  ; ; ; ; ;   f u l l - a c k        ` ( a c k - s e p a r a t o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( a c k - f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( a c k - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( a c k - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g - 1   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   g i t - a n n e x        ' ( g i t - a n n e x - d i r e d - a n n e x e d - a v a i l a b l e   ( ( t   ( : i n h e r i t   s u c c e s s   : w e i g h t   n o r m a l ) ) ) )        ' ( g i t - a n n e x - d i r e d - a n n e x e d - u n a v a i l a b l e   ( ( t   ( : i n h e r i t   e r r o r   : w e i g h t   n o r m a l ) ) ) )  ; ; ; ; ;   g i t - c o m m i t        ` ( g i t - c o m m i t - c o m m e n t - a c t i o n     ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1   : w e i g h t   b o l d ) ) ) )        ` ( g i t - c o m m i t - c o m m e n t - b r a n c h     ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1     : w e i g h t   b o l d ) ) ) )   ;   o b s o l e t e        ` ( g i t - c o m m i t - c o m m e n t - b r a n c h - l o c a l     ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1     : w e i g h t   b o l d ) ) ) )        ` ( g i t - c o m m i t - c o m m e n t - b r a n c h - r e m o t e   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n     : w e i g h t   b o l d ) ) ) )        ` ( g i t - c o m m i t - c o m m e n t - h e a d i n g   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w     : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   g i t - g u t t e r        ` ( g i t - g u t t e r : a d d e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   b o l d   : i n v e r s e - v i d e o   t ) ) ) )        ` ( g i t - g u t t e r : d e l e t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d   : i n v e r s e - v i d e o   t ) ) ) )        ` ( g i t - g u t t e r : m o d i f i e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : w e i g h t   b o l d   : i n v e r s e - v i d e o   t ) ) ) )        ` ( g i t - g u t t e r : u n c h a n g e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d   : i n v e r s e - v i d e o   t ) ) ) )  ; ; ; ; ;   g i t - g u t t e r - f r        ` ( g i t - g u t t e r - f r : a d d e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n     : w e i g h t   b o l d ) ) ) )        ` ( g i t - g u t t e r - f r : d e l e t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( g i t - g u t t e r - f r : m o d i f i e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   g i t - r e b a s e        ` ( g i t - r e b a s e - h a s h   ( ( t   ( : f o r e g r o u n d ,   z e n b u r n - o r a n g e ) ) ) )  ; ; ; ; ;   g n u s        ` ( g n u s - g r o u p - m a i l - 1   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 1 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 1 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 1 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 2   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 2 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 2 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 2 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 3   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 3 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 3 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 3 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 4   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 4 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 4 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 4 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 5   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 5 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 5 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 5 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 6   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - 6 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - 6 - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - 6 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - l o w   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - m a i l - l o w - e m p t y ) ) ) )        ` ( g n u s - g r o u p - m a i l - l o w - e m p t y   ( ( t   ( : i n h e r i t   g n u s - g r o u p - n e w s - l o w - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 1   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 1 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 2   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 2 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 3   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 3 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 4   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 4 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 5   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 5 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - 6   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - 6 - e m p t y ) ) ) )        ` ( g n u s - g r o u p - n e w s - l o w   ( ( t   ( : w e i g h t   b o l d   : i n h e r i t   g n u s - g r o u p - n e w s - l o w - e m p t y ) ) ) )        ` ( g n u s - h e a d e r - c o n t e n t   ( ( t   ( : i n h e r i t   m e s s a g e - h e a d e r - o t h e r ) ) ) )        ` ( g n u s - h e a d e r - f r o m   ( ( t   ( : i n h e r i t   m e s s a g e - h e a d e r - t o ) ) ) )        ` ( g n u s - h e a d e r - n a m e   ( ( t   ( : i n h e r i t   m e s s a g e - h e a d e r - n a m e ) ) ) )        ` ( g n u s - h e a d e r - n e w s g r o u p s   ( ( t   ( : i n h e r i t   m e s s a g e - h e a d e r - o t h e r ) ) ) )        ` ( g n u s - h e a d e r - s u b j e c t   ( ( t   ( : i n h e r i t   m e s s a g e - h e a d e r - s u b j e c t ) ) ) )        ` ( g n u s - s e r v e r - o p e n e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s e r v e r - d e n i e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s e r v e r - c l o s e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : s l a n t   i t a l i c ) ) ) )        ` ( g n u s - s e r v e r - o f f l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s e r v e r - a g e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - c a n c e l l e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( g n u s - s u m m a r y - h i g h - a n c i e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g n u s - s u m m a r y - h i g h - r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - h i g h - t i c k e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - h i g h - u n r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - l o w - a n c i e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g n u s - s u m m a r y - l o w - r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( g n u s - s u m m a r y - l o w - t i c k e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - l o w - u n r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( g n u s - s u m m a r y - n o r m a l - a n c i e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g n u s - s u m m a r y - n o r m a l - r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( g n u s - s u m m a r y - n o r m a l - t i c k e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - s u m m a r y - n o r m a l - u n r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( g n u s - s u m m a r y - s e l e c t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( g n u s - c i t e - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g n u s - c i t e - 1 0   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( g n u s - c i t e - 1 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( g n u s - c i t e - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( g n u s - c i t e - 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( g n u s - c i t e - 4   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( g n u s - c i t e - 5   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( g n u s - c i t e - 6   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( g n u s - c i t e - 7   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( g n u s - c i t e - 8   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( g n u s - c i t e - 9   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 2 ) ) ) )        ` ( g n u s - g r o u p - n e w s - 1 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( g n u s - g r o u p - n e w s - 2 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( g n u s - g r o u p - n e w s - 3 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( g n u s - g r o u p - n e w s - 4 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( g n u s - g r o u p - n e w s - 5 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 3 ) ) ) )        ` ( g n u s - g r o u p - n e w s - 6 - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( g n u s - g r o u p - n e w s - l o w - e m p t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( g n u s - s i g n a t u r e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( g n u s - x   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - f g   : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( m m - u u - e x t r a c t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5   : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )  ; ; ; ; ;   g o - g u r u        ` ( g o - g u r u - h l - i d e n t i f i e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g - 1   : b a c k g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )  ; ; ; ; ;   g u i d e - k e y        ` ( g u i d e - k e y / h i g h l i g h t - c o m m a n d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( g u i d e - k e y / k e y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( g u i d e - k e y / p r e f i x - c o m m a n d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )  ; ; ; ; ;   h a c k e r n e w s        ' ( h a c k e r n e w s - c o m m e n t - c o u n t   ( ( t   ( : i n h e r i t   l i n k - v i s i t e d   : u n d e r l i n e   n i l ) ) ) )        ' ( h a c k e r n e w s - l i n k                     ( ( t   ( : i n h e r i t   l i n k                   : u n d e r l i n e   n i l ) ) ) )  ; ; ; ; ;   h e l m        ` ( h e l m - h e a d e r            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n                                              : b a c k g r o u n d   , z e n b u r n - b g                                              : u n d e r l i n e   n i l                                              : b o x   n i l ) ) ) )        ` ( h e l m - s o u r c e - h e a d e r            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w                                              : b a c k g r o u n d   , z e n b u r n - b g - 1                                              : u n d e r l i n e   n i l                                              : w e i g h t   b o l d                                              : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   r e l e a s e d - b u t t o n ) ) ) ) )        ` ( h e l m - s e l e c t i o n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1   : u n d e r l i n e   n i l ) ) ) )        ` ( h e l m - s e l e c t i o n - l i n e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( h e l m - v i s i b l e - m a r k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( h e l m - c a n d i d a t e - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( h e l m - s e p a r a t o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - t i m e - z o n e - c u r r e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - t i m e - z o n e - h o m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b o o k m a r k - a d d r e s s b o o k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b o o k m a r k - d i r e c t o r y   ( ( t   ( : f o r e g r o u n d   n i l   : b a c k g r o u n d   n i l   : i n h e r i t   h e l m - f f - d i r e c t o r y ) ) ) )        ` ( h e l m - b o o k m a r k - f i l e   ( ( t   ( : f o r e g r o u n d   n i l   : b a c k g r o u n d   n i l   : i n h e r i t   h e l m - f f - f i l e ) ) ) )        ` ( h e l m - b o o k m a r k - g n u s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b o o k m a r k - i n f o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b o o k m a r k - m a n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b o o k m a r k - w 3 m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b u f f e r - n o t - s a v e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b u f f e r - p r o c e s s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b u f f e r - s a v e d - o u t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - b u f f e r - s i z e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - f f - d i r e c t o r y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )        ` ( h e l m - f f - f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   n o r m a l ) ) ) )        ` ( h e l m - f f - e x e c u t a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   n o r m a l ) ) ) )        ` ( h e l m - f f - i n v a l i d - s y m l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )        ` ( h e l m - f f - s y m l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )        ` ( h e l m - f f - p r e f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   n o r m a l ) ) ) )        ` ( h e l m - g r e p - c m d - l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - g r e p - f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - g r e p - f i n i s h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - g r e p - l i n e n o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - g r e p - m a t c h   ( ( t   ( : f o r e g r o u n d   n i l   : b a c k g r o u n d   n i l   : i n h e r i t   h e l m - m a t c h ) ) ) )        ` ( h e l m - g r e p - r u n n i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : b a c k g r o u n d   , z e n b u r n - b g - 1   : w e i g h t   b o l d ) ) ) )        ` ( h e l m - m o c c u r - b u f f e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - m u - c o n t a c t s - a d d r e s s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h e l m - m u - c o n t a c t s - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   h e l m - s w o o p        ` ( h e l m - s w o o p - t a r g e t - l i n e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( h e l m - s w o o p - t a r g e t - w o r d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g + 2   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   h l - l i n e - m o d e        ` ( h l - l i n e - f a c e   ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) )                                        ( t   : w e i g h t   b o l d ) ) )        ` ( h l - l i n e   ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) )   ;   o l d   e m a c s e n                              ( t   : w e i g h t   b o l d ) ) )  ; ; ; ; ;   h l - s e x p        ` ( h l - s e x p - f a c e   ( ( , c l a s s   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) )                                        ( t   : w e i g h t   b o l d ) ) )  ; ; ; ; ;   h y d r a        ` ( h y d r a - f a c e - r e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h y d r a - f a c e - a m a r a n t h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 3   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h y d r a - f a c e - b l u e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h y d r a - f a c e - p i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( h y d r a - f a c e - t e a l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   i n f o +        ` ( i n f o - c o m m a n d - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( i n f o - c o n s t a n t - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( i n f o - d o u b l e - q u o t e d - n a m e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( i n f o - f i l e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( i n f o - f u n c t i o n - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e ) ) ) )        ` ( i n f o - m a c r o - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( i n f o - m e n u   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( i n f o - q u o t e d - n a m e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( i n f o - r e f e r e n c e - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( i n f o - s i n g l e - q u o t e   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ` ( i n f o - s p e c i a l - f o r m - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( i n f o - s t r i n g   ( ( t   ( : i n h e r i t   f o n t - l o c k - s t r i n g - f a c e ) ) ) )        ` ( i n f o - s y n t a x - c l a s s - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( i n f o - u s e r - o p t i o n - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( i n f o - v a r i a b l e - r e f - i t e m   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )  ; ; ; ; ;   i r f c        ` ( i r f c - h e a d - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( i r f c - h e a d - n u m b e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( i r f c - r e f e r e n c e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1   : w e i g h t   b o l d ) ) ) )        ` ( i r f c - r e q u i r e m e n t - k e y w o r d - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ` ( i r f c - r f c - l i n k - f a c e   ( ( t   ( : i n h e r i t   l i n k ) ) ) )        ` ( i r f c - r f c - n u m b e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )        ` ( i r f c - s t d - n u m b e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4   : w e i g h t   b o l d ) ) ) )        ` ( i r f c - t a b l e - i t e m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( i r f c - t i t l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w                                                                              : u n d e r l i n e   t   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   i v y        ` ( i v y - c o n f i r m - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( i v y - c u r r e n t - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d   : u n d e r l i n e   t ) ) ) )        ` ( i v y - c u r s o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( i v y - m a t c h - r e q u i r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( i v y - m i n i b u f f e r - m a t c h - f a c e - 1   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( i v y - m i n i b u f f e r - m a t c h - f a c e - 2   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( i v y - m i n i b u f f e r - m a t c h - f a c e - 3   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( i v y - m i n i b u f f e r - m a t c h - f a c e - 4   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( i v y - r e m o t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( i v y - s u b d i r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   i d o - m o d e        ` ( i d o - f i r s t - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( i d o - o n l y - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( i d o - s u b d i r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( i d o - i n d i c a t o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )  ; ; ; ; ;   i e d i t - m o d e        ` ( i e d i t - o c c u r r e n c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   j a b b e r - m o d e        ` ( j a b b e r - r o s t e r - u s e r - a w a y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( j a b b e r - r o s t e r - u s e r - o n l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( j a b b e r - r o s t e r - u s e r - d n d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( j a b b e r - r o s t e r - u s e r - x a   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( j a b b e r - r o s t e r - u s e r - c h a t t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( j a b b e r - r o s t e r - u s e r - e r r o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( j a b b e r - r a r e - t i m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( j a b b e r - c h a t - p r o m p t - l o c a l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( j a b b e r - c h a t - p r o m p t - f o r e i g n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( j a b b e r - c h a t - p r o m p t - s y s t e m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( j a b b e r - a c t i v i t y - f a c e ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( j a b b e r - a c t i v i t y - p e r s o n a l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( j a b b e r - t i t l e - s m a l l   ( ( t   ( : h e i g h t   1 . 1   : w e i g h t   b o l d ) ) ) )        ` ( j a b b e r - t i t l e - m e d i u m   ( ( t   ( : h e i g h t   1 . 2   : w e i g h t   b o l d ) ) ) )        ` ( j a b b e r - t i t l e - l a r g e   ( ( t   ( : h e i g h t   1 . 3   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   j s 2 - m o d e        ` ( j s 2 - w a r n i n g   ( ( t   ( : u n d e r l i n e   , z e n b u r n - o r a n g e ) ) ) )        ` ( j s 2 - e r r o r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( j s 2 - j s d o c - t a g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( j s 2 - j s d o c - t y p e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( j s 2 - j s d o c - v a l u e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( j s 2 - f u n c t i o n - p a r a m   ( ( t   ( : f o r e g r o u n d ,   z e n b u r n - o r a n g e ) ) ) )        ` ( j s 2 - e x t e r n a l - v a r i a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )  ; ; ; ; ;   a d d i t i o n a l   j s 2   m o d e   a t t r i b u t e s   f o r   b e t t e r   s y n t a x   h i g h l i g h t i n g        ` ( j s 2 - i n s t a n c e - m e m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )        ` ( j s 2 - j s d o c - h t m l - t a g - d e l i m i t e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( j s 2 - j s d o c - h t m l - t a g - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( j s 2 - o b j e c t - p r o p e r t y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( j s 2 - m a g i c - p a r e n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( j s 2 - p r i v a t e - f u n c t i o n - c a l l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( j s 2 - f u n c t i o n - c a l l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( j s 2 - p r i v a t e - m e m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( j s 2 - k e y w o r d s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )  ; ; ; ; ;   l e d g e r - m o d e        ` ( l e d g e r - f o n t - p a y e e - u n c l e a r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d ) ) ) )        ` ( l e d g e r - f o n t - p a y e e - c l e a r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - p a y e e - p e n d i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - x a c t - h i g h l i g h t - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( l e d g e r - f o n t - a u t o - x a c t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - p e r i o d i c - x a c t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - p e n d i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   w e i g h t :   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - o t h e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( l e d g e r - f o n t - p o s t i n g - d a t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - p o s t i n g - a c c o u n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( l e d g e r - f o n t - p o s t i n g - a c c o u n t - c l e a r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( l e d g e r - f o n t - p o s t i n g - a c c o u n t - p e n d i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( l e d g e r - f o n t - p o s t i n g - a m o u n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( l e d g e r - o c c u r - n a r r o w e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : i n v i s i b l e   t ) ) ) )        ` ( l e d g e r - o c c u r - x a c t - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( l e d g e r - f o n t - c o m m e n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( l e d g e r - f o n t - r e c o n c i l e r - u n c l e a r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d ) ) ) )        ` ( l e d g e r - f o n t - r e c o n c i l e r - c l e a r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - r e c o n c i l e r - p e n d i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   n o r m a l ) ) ) )        ` ( l e d g e r - f o n t - r e p o r t - c l i c k a b l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   n o r m a l ) ) ) )  ; ; ; ; ;   l i n u m - m o d e        ` ( l i n u m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   l i s p y        ` ( l i s p y - c o m m a n d - n a m e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5   : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e ) ) ) )        ` ( l i s p y - c u r s o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( l i s p y - f a c e - h i n t   ( ( t   ( : i n h e r i t   h i g h l i g h t   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   r u l e r - m o d e        ` ( r u l e r - m o d e - c o l u m n - n u m b e r   ( ( t   ( : i n h e r i t   ' r u l e r - m o d e - d e f a u l t   : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( r u l e r - m o d e - f i l l - c o l u m n   ( ( t   ( : i n h e r i t   ' r u l e r - m o d e - d e f a u l t   : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r u l e r - m o d e - g o a l - c o l u m n   ( ( t   ( : i n h e r i t   ' r u l e r - m o d e - f i l l - c o l u m n ) ) ) )        ` ( r u l e r - m o d e - c o m m e n t - c o l u m n   ( ( t   ( : i n h e r i t   ' r u l e r - m o d e - f i l l - c o l u m n ) ) ) )        ` ( r u l e r - m o d e - t a b - s t o p   ( ( t   ( : i n h e r i t   ' r u l e r - m o d e - f i l l - c o l u m n ) ) ) )        ` ( r u l e r - m o d e - c u r r e n t - c o l u m n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b o x   t ) ) ) )        ` ( r u l e r - m o d e - d e f a u l t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )   ; ; ; ; ;   l u i        ` ( l u i - t i m e - s t a m p - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( l u i - h i l i g h t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( l u i - b u t t o n - f a c e   ( ( t   ( : i n h e r i t   h o v e r - h i g h l i g h t ) ) ) )  ; ; ; ; ;   m a c r o s t e p        ` ( m a c r o s t e p - g e n s y m - 1            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m a c r o s t e p - g e n s y m - 2            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m a c r o s t e p - g e n s y m - 3            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m a c r o s t e p - g e n s y m - 4            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m a c r o s t e p - g e n s y m - 5            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m a c r o s t e p - e x p a n s i o n - h i g h l i g h t - f a c e            ( ( t   ( : i n h e r i t   h i g h l i g h t ) ) ) )        ` ( m a c r o s t e p - m a c r o - f a c e            ( ( t   ( : u n d e r l i n e   t ) ) ) )  ; ; ; ; ;   m a g i t  ; ; ; ; ; ;   h e a d i n g s   a n d   d i f f s        ` ( m a g i t - s e c t i o n - h i g h l i g h t                       ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 0 5 ) ) ) )        ` ( m a g i t - s e c t i o n - h e a d i n g                           ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - s e c t i o n - h e a d i n g - s e l e c t i o n       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )         ` ( m a g i t - d i f f - a d d e d   ( ( t   ( : i n h e r i t   d i f f - a d d e d ) ) ) )        ` ( m a g i t - d i f f - a d d e d - h i g h l i g h t   ( ( t   ( : i n h e r i t   d i f f - r e f i n e - a d d e d ) ) ) )        ` ( m a g i t - d i f f - r e m o v e d   ( ( t   ( : i n h e r i t   d i f f - r e m o v e d ) ) ) )        ` ( m a g i t - d i f f - r e m o v e d - h i g h l i g h t   ( ( t   ( : i n h e r i t   d i f f - r e f i n e - r e m o v e d ) ) ) )         ` ( m a g i t - d i f f - f i l e - h e a d i n g                       ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - d i f f - f i l e - h e a d i n g - h i g h l i g h t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 0 5     : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - d i f f - f i l e - h e a d i n g - s e l e c t i o n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 0 5                                                                                                                  : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - d i f f - h u n k - h e a d i n g                       ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( m a g i t - d i f f - h u n k - h e a d i n g - h i g h l i g h t   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( m a g i t - d i f f - h u n k - h e a d i n g - s e l e c t i o n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2                                                                                                                  : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m a g i t - d i f f - l i n e s - h e a d i n g                     ( ( t   ( : b a c k g r o u n d   , z e n b u r n - o r a n g e                                                                                                                  : f o r e g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( m a g i t - d i f f - c o n t e x t - h i g h l i g h t             ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 0 5                                                                                                                  : f o r e g r o u n d   " g r e y 7 0 " ) ) ) )        ` ( m a g i t - d i f f s t a t - a d d e d       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( m a g i t - d i f f s t a t - r e m o v e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ; ;   p o p u p        ` ( m a g i t - p o p u p - h e a d i n g                           ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w     : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - p o p u p - k e y                                   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - p o p u p - a r g u m e n t                         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n       : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - p o p u p - d i s a b l e d - a r g u m e n t       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1         : w e i g h t   n o r m a l ) ) ) )        ` ( m a g i t - p o p u p - o p t i o n - v a l u e                 ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2     : w e i g h t   b o l d ) ) ) )  ; ; ; ; ; ;   p r o c e s s        ` ( m a g i t - p r o c e s s - o k         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n     : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - p r o c e s s - n g         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d         : w e i g h t   b o l d ) ) ) )  ; ; ; ; ; ;   l o g        ` ( m a g i t - l o g - a u t h o r         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m a g i t - l o g - d a t e             ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )        ` ( m a g i t - l o g - g r a p h           ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g + 1 ) ) ) )  ; ; ; ; ; ;   s e q u e n c e        ` ( m a g i t - s e q u e n c e - p i c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( m a g i t - s e q u e n c e - s t o p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - s e q u e n c e - p a r t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a g i t - s e q u e n c e - h e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m a g i t - s e q u e n c e - d r o p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m a g i t - s e q u e n c e - d o n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )        ` ( m a g i t - s e q u e n c e - o n t o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )  ; ; ; ; ; ;   b i s e c t        ` ( m a g i t - b i s e c t - g o o d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - b i s e c t - s k i p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a g i t - b i s e c t - b a d     ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ; ;   b l a m e        ` ( m a g i t - b l a m e - h e a d i n g   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( m a g i t - b l a m e - h a s h         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( m a g i t - b l a m e - n a m e         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m a g i t - b l a m e - d a t e         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m a g i t - b l a m e - s u m m a r y   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - b l u e - 2                                                                                      : w e i g h t   b o l d ) ) ) )  ; ; ; ; ; ;   r e f e r e n c e s   e t c        ` ( m a g i t - d i m m e d                   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( m a g i t - h a s h                       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( m a g i t - t a g                         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - b r a n c h - r e m o t e     ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n     : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - b r a n c h - l o c a l       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e       : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - b r a n c h - c u r r e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e       : w e i g h t   b o l d   : b o x   t ) ) ) )        ` ( m a g i t - h e a d                       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e       : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - r e f n a m e                 ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - r e f n a m e - s t a s h     ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - r e f n a m e - w i p         ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( m a g i t - s i g n a t u r e - g o o d             ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - s i g n a t u r e - b a d               ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m a g i t - s i g n a t u r e - u n t r u s t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a g i t - s i g n a t u r e - e x p i r e d       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m a g i t - s i g n a t u r e - r e v o k e d       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ' ( m a g i t - s i g n a t u r e - e r r o r           ( ( t   ( : i n h e r i t         m a g i t - s i g n a t u r e - b a d ) ) ) )        ` ( m a g i t - c h e r r y - u n m a t c h e d         ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( m a g i t - c h e r r y - e q u i v a l e n t       ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( m a g i t - r e f l o g - c o m m i t               ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - r e f l o g - a m e n d                 ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( m a g i t - r e f l o g - m e r g e                 ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - r e f l o g - c h e c k o u t           ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m a g i t - r e f l o g - r e s e t                 ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m a g i t - r e f l o g - r e b a s e               ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( m a g i t - r e f l o g - c h e r r y - p i c k     ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m a g i t - r e f l o g - r e m o t e               ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( m a g i t - r e f l o g - o t h e r                 ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )  ; ; ; ; ;   m a r k u p - f a c e s        ` ( m a r k u p - a n c h o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( m a r k u p - c o d e - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( m a r k u p - c o m m a n d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a r k u p - e m p h a s i s - f a c e   ( ( t   ( : i n h e r i t   b o l d ) ) ) )        ` ( m a r k u p - i n t e r n a l - r e f e r e n c e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : u n d e r l i n e   t ) ) ) )        ` ( m a r k u p - l i s t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g + 1 ) ) ) )        ` ( m a r k u p - m e t a - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a r k u p - m e t a - h i d e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m a r k u p - s e c o n d a r y - t e x t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( m a r k u p - t i t l e - 0 - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e   : w e i g h t   b o l d ) ) ) )        ` ( m a r k u p - t i t l e - 1 - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e   : w e i g h t   b o l d ) ) ) )        ` ( m a r k u p - t i t l e - 2 - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e   : w e i g h t   b o l d ) ) ) )        ` ( m a r k u p - t i t l e - 3 - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e   : w e i g h t   b o l d ) ) ) )        ` ( m a r k u p - t i t l e - 4 - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - f u n c t i o n - n a m e - f a c e   : w e i g h t   b o l d ) ) ) )        ` ( m a r k u p - t y p e w r i t e r - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( m a r k u p - v e r b a t i m - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( m a r k u p - v a l u e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   m e s s a g e - m o d e        ` ( m e s s a g e - c i t e d - t e x t   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( m e s s a g e - h e a d e r - n a m e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( m e s s a g e - h e a d e r - o t h e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e s s a g e - h e a d e r - t o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m e s s a g e - h e a d e r - c c   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m e s s a g e - h e a d e r - n e w s g r o u p s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m e s s a g e - h e a d e r - s u b j e c t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( m e s s a g e - h e a d e r - x h e a d e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e s s a g e - m m l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( m e s s a g e - s e p a r a t o r   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )  ; ; ; ; ;   m e w        ` ( m e w - f a c e - h e a d e r - s u b j e c t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m e w - f a c e - h e a d e r - f r o m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m e w - f a c e - h e a d e r - d a t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - h e a d e r - t o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m e w - f a c e - h e a d e r - k e y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - h e a d e r - p r i v a t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - h e a d e r - i m p o r t a n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m e w - f a c e - h e a d e r - m a r g i n a l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( m e w - f a c e - h e a d e r - w a r n i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m e w - f a c e - h e a d e r - x m e w   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - h e a d e r - x m e w - b a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m e w - f a c e - b o d y - u r l   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m e w - f a c e - b o d y - c o m m e n t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : s l a n t   i t a l i c ) ) ) )        ` ( m e w - f a c e - b o d y - c i t e 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - b o d y - c i t e 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m e w - f a c e - b o d y - c i t e 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( m e w - f a c e - b o d y - c i t e 4   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m e w - f a c e - b o d y - c i t e 5   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m e w - f a c e - m a r k - r e v i e w   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m e w - f a c e - m a r k - e s c a p e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - m a r k - d e l e t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( m e w - f a c e - m a r k - u n l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m e w - f a c e - m a r k - r e f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - m a r k - u n r e a d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 2 ) ) ) )        ` ( m e w - f a c e - e o f - m e s s a g e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( m e w - f a c e - e o f - p a r t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   m i c - p a r e n        ` ( p a r e n - f a c e - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : b a c k g r o u n d   , z e n b u r n - b g   : w e i g h t   b o l d ) ) ) )        ` ( p a r e n - f a c e - m i s m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a   : w e i g h t   b o l d ) ) ) )        ` ( p a r e n - f a c e - n o - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   m i n g u s        ` ( m i n g u s - d i r e c t o r y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( m i n g u s - p a u s i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( m i n g u s - p l a y i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( m i n g u s - p l a y l i s t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   ) ) ) )        ` ( m i n g u s - m a r k - f a c e   ( ( t   ( : b o l d   t   : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( m i n g u s - s o n g - f i l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( m i n g u s - a r t i s t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( m i n g u s - a l b u m - f a c e   ( ( t   ( : u n d e r l i n e   t   : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( m i n g u s - a l b u m - s t a l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( m i n g u s - s t o p p e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   n a v        ` ( n a v - f a c e - h e a d i n g   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( n a v - f a c e - b u t t o n - n u m   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( n a v - f a c e - d i r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( n a v - f a c e - h d i r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( n a v - f a c e - f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n a v - f a c e - h f i l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )  ; ; ; ; ;   m e r l i n        ` ( m e r l i n - t y p e - f a c e   ( ( t   ( : i n h e r i t   h i g h l i g h t ) ) ) )        ` ( m e r l i n - c o m p i l a t i o n - w a r n i n g - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - o r a n g e ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - o r a n g e ) ) ) )        ` ( m e r l i n - c o m p i l a t i o n - e r r o r - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - r e d ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   m u 4 e        ` ( m u 4 e - c i t e d - 1 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e         : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 2 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2   : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 3 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2     : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 4 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n       : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 5 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 4     : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 6 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2   : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - c i t e d - 7 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e         : s l a n t   i t a l i c ) ) ) )        ` ( m u 4 e - r e p l i e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( m u 4 e - t r a s h e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 3   : s t r i k e - t h r o u g h   t ) ) ) )  ; ; ; ; ;   m u m a m o        ` ( m u m a m o - b a c k g r o u n d - c h u n k - m a j o r   ( ( t   ( : b a c k g r o u n d   n i l ) ) ) )        ` ( m u m a m o - b a c k g r o u n d - c h u n k - s u b m o d e 1   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( m u m a m o - b a c k g r o u n d - c h u n k - s u b m o d e 2   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( m u m a m o - b a c k g r o u n d - c h u n k - s u b m o d e 3   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 3 ) ) ) )        ` ( m u m a m o - b a c k g r o u n d - c h u n k - s u b m o d e 4   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1 ) ) ) )  ; ; ; ; ;   n e o t r e e        ` ( n e o - b a n n e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : w e i g h t   b o l d ) ) ) )        ` ( n e o - h e a d e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e o - r o o t - d i r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : w e i g h t   b o l d ) ) ) )        ` ( n e o - d i r - l i n k - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( n e o - f i l e - l i n k - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e o - e x p a n d - b t n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( n e o - v c - d e f a u l t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g + 1 ) ) ) )        ` ( n e o - v c - u s e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : s l a n t   i t a l i c ) ) ) )        ` ( n e o - v c - u p - t o - d a t e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( n e o - v c - e d i t e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( n e o - v c - n e e d s - m e r g e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( n e o - v c - u n l o c k e d - c h a n g e s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( n e o - v c - a d d e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( n e o - v c - c o n f l i c t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( n e o - v c - m i s s i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( n e o - v c - i g n o r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )  ; ; ; ; ;   o r g - m o d e        ` ( o r g - a g e n d a - d a t e - t o d a y            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g + 1   : s l a n t   i t a l i c   : w e i g h t   b o l d ) ) )   t )        ` ( o r g - a g e n d a - s t r u c t u r e            ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( o r g - a r c h i v e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( o r g - c h e c k b o x   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : f o r e g r o u n d   , z e n b u r n - f g + 1                                                                        : b o x   ( : l i n e - w i d t h   1   : s t y l e   r e l e a s e d - b u t t o n ) ) ) ) )        ` ( o r g - d a t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : u n d e r l i n e   t ) ) ) )        ` ( o r g - d e a d l i n e - a n n o u n c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( o r g - d o n e   ( ( t   ( : w e i g h t   b o l d   : w e i g h t   b o l d   : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( o r g - f o r m u l a   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( o r g - h e a d l i n e - d o n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( o r g - h i d e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( o r g - l e v e l - 1   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - o r a n g e                                                                , @ ( w h e n   z e n b u r n - s c a l e - o r g - h e a d l i n e s                                                                        ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 4 ) ) ) ) ) )        ` ( o r g - l e v e l - 2   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - g r e e n + 4                                                                , @ ( w h e n   z e n b u r n - s c a l e - o r g - h e a d l i n e s                                                                        ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 3 ) ) ) ) ) )        ` ( o r g - l e v e l - 3   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - b l u e - 1                                                                , @ ( w h e n   z e n b u r n - s c a l e - o r g - h e a d l i n e s                                                                        ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 2 ) ) ) ) ) )        ` ( o r g - l e v e l - 4   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2                                                                , @ ( w h e n   z e n b u r n - s c a l e - o r g - h e a d l i n e s                                                                        ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 1 ) ) ) ) ) )        ` ( o r g - l e v e l - 5   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( o r g - l e v e l - 6   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( o r g - l e v e l - 7   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( o r g - l e v e l - 8   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - b l u e - 4 ) ) ) )        ` ( o r g - l i n k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : u n d e r l i n e   t ) ) ) )        ` ( o r g - s c h e d u l e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( o r g - s c h e d u l e d - p r e v i o u s l y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( o r g - s c h e d u l e d - t o d a y   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( o r g - s e x p - d a t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : u n d e r l i n e   t ) ) ) )        ` ( o r g - s p e c i a l - k e y w o r d   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( o r g - t a b l e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( o r g - t a g   ( ( t   ( : w e i g h t   b o l d   : w e i g h t   b o l d ) ) ) )        ` ( o r g - t i m e - g r i d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( o r g - t o d o   ( ( t   ( : w e i g h t   b o l d   : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( o r g - u p c o m i n g - d e a d l i n e   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ` ( o r g - w a r n i n g   ( ( t   ( : w e i g h t   b o l d   : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d   : u n d e r l i n e   n i l ) ) ) )        ` ( o r g - c o l u m n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( o r g - c o l u m n - t i t l e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : u n d e r l i n e   t   : w e i g h t   b o l d ) ) ) )        ` ( o r g - m o d e - l i n e - c l o c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( o r g - m o d e - l i n e - c l o c k - o v e r r u n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( o r g - e l l i p s i s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1   : u n d e r l i n e   t ) ) ) )        ` ( o r g - f o o t n o t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : u n d e r l i n e   t ) ) ) )        ` ( o r g - d o c u m e n t - t i t l e   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - b l u e                                                                                    : w e i g h t   b o l d   : h e i g h t   , z e n b u r n - h e i g h t - p l u s - 4 ) ) ) )        ` ( o r g - d o c u m e n t - i n f o   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( o r g - h a b i t - r e a d y - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - g r e e n ) ) )        ` ( o r g - h a b i t - a l e r t - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - y e l l o w - 1   : f o r e g r o u n d   , z e n b u r n - b g ) ) )        ` ( o r g - h a b i t - c l e a r - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - b l u e - 3 ) ) )        ` ( o r g - h a b i t - o v e r d u e - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - r e d - 3 ) ) )        ` ( o r g - h a b i t - c l e a r - f u t u r e - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - b l u e - 4 ) ) )        ` ( o r g - h a b i t - r e a d y - f u t u r e - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - g r e e n - 2 ) ) )        ` ( o r g - h a b i t - a l e r t - f u t u r e - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - y e l l o w - 2   : f o r e g r o u n d   , z e n b u r n - b g ) ) )        ` ( o r g - h a b i t - o v e r d u e - f u t u r e - f a c e   ( ( t   : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) )  ; ; ; ; ;   o r g - r e f        ` ( o r g - r e f - r e f - f a c e   ( ( t   : u n d e r l i n e   t ) ) )        ` ( o r g - r e f - l a b e l - f a c e   ( ( t   : u n d e r l i n e   t ) ) )        ` ( o r g - r e f - c i t e - f a c e   ( ( t   : u n d e r l i n e   t ) ) )        ` ( o r g - r e f - g l o s s a r y - f a c e   ( ( t   : u n d e r l i n e   t ) ) )        ` ( o r g - r e f - a c r o n y m - f a c e   ( ( t   : u n d e r l i n e   t ) ) )  ; ; ; ; ;   o u t l i n e        ` ( o u t l i n e - 1   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - o r a n g e                                                            , @ ( w h e n   z e n b u r n - s c a l e - o u t l i n e - h e a d l i n e s                                                                    ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 4 ) ) ) ) ) )        ` ( o u t l i n e - 2   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - g r e e n + 4                                                            , @ ( w h e n   z e n b u r n - s c a l e - o u t l i n e - h e a d l i n e s                                                                    ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 3 ) ) ) ) ) )        ` ( o u t l i n e - 3   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - b l u e - 1                                                            , @ ( w h e n   z e n b u r n - s c a l e - o u t l i n e - h e a d l i n e s                                                                    ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 2 ) ) ) ) ) )        ` ( o u t l i n e - 4   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2                                                            , @ ( w h e n   z e n b u r n - s c a l e - o u t l i n e - h e a d l i n e s                                                                    ( l i s t   : h e i g h t   z e n b u r n - h e i g h t - p l u s - 1 ) ) ) ) ) )        ` ( o u t l i n e - 5   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( o u t l i n e - 6   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( o u t l i n e - 7   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( o u t l i n e - 8   ( ( t   ( : i n h e r i t   , z - v a r i a b l e - p i t c h   : f o r e g r o u n d   , z e n b u r n - b l u e - 4 ) ) ) )  ; ; ; ; ;   p 4        ` ( p 4 - d e p o t - a d d e d - f a c e   ( ( t   : i n h e r i t   d i f f - a d d e d ) ) )        ` ( p 4 - d e p o t - b r a n c h - o p - f a c e   ( ( t   : i n h e r i t   d i f f - c h a n g e d ) ) )        ` ( p 4 - d e p o t - d e l e t e d - f a c e   ( ( t   : i n h e r i t   d i f f - r e m o v e d ) ) )        ` ( p 4 - d e p o t - u n m a p p e d - f a c e   ( ( t   : i n h e r i t   d i f f - c h a n g e d ) ) )        ` ( p 4 - d i f f - c h a n g e - f a c e   ( ( t   : i n h e r i t   d i f f - c h a n g e d ) ) )        ` ( p 4 - d i f f - d e l - f a c e   ( ( t   : i n h e r i t   d i f f - r e m o v e d ) ) )        ` ( p 4 - d i f f - f i l e - f a c e   ( ( t   : i n h e r i t   d i f f - f i l e - h e a d e r ) ) )        ` ( p 4 - d i f f - h e a d - f a c e   ( ( t   : i n h e r i t   d i f f - h e a d e r ) ) )        ` ( p 4 - d i f f - i n s - f a c e   ( ( t   : i n h e r i t   d i f f - a d d e d ) ) )  ; ; ; ; ;   c / p e r l        ` ( c p e r l - n o n o v e r r i d a b l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( c p e r l - a r r a y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ,   : b a c k g o r u n d   , z e n b u r n - b g ) ) ) )        ` ( c p e r l - h a s h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ,   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )  ; ; ; ; ;   p a r e n - f a c e        ` ( p a r e n t h e s i s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g - 1 ) ) ) )  ; ; ; ; ;   p e r s p e c t i v e        ` ( p e r s p - s e l e c t e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : i n h e r i t   m o d e - l i n e ) ) ) )  ; ; ; ; ;   p o w e r l i n e        ` ( p o w e r l i n e - a c t i v e 1   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5   : i n h e r i t   m o d e - l i n e ) ) ) )        ` ( p o w e r l i n e - a c t i v e 2   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 2   : i n h e r i t   m o d e - l i n e ) ) ) )        ` ( p o w e r l i n e - i n a c t i v e 1   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1   : i n h e r i t   m o d e - l i n e - i n a c t i v e ) ) ) )        ` ( p o w e r l i n e - i n a c t i v e 2   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 3   : i n h e r i t   m o d e - l i n e - i n a c t i v e ) ) ) )  ; ; ; ; ;   p r o o f g e n e r a l        ` ( p r o o f - a c t i v e - a r e a - f a c e   ( ( t   ( : u n d e r l i n e   t ) ) ) )        ` ( p r o o f - b o r i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 2 ) ) ) )        ` ( p r o o f - c o m m a n d - m o u s e - h i g h l i g h t - f a c e   ( ( t   ( : i n h e r i t   p r o o f - m o u s e - h i g h l i g h t - f a c e ) ) ) )        ` ( p r o o f - d e b u g - m e s s a g e - f a c e   ( ( t   ( : i n h e r i t   p r o o f - b o r i n g - f a c e ) ) ) )        ` ( p r o o f - d e c l a r a t i o n - n a m e - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - k e y w o r d - f a c e   : f o r e g r o u n d   n i l ) ) ) )        ` ( p r o o f - e a g e r - a n n o t a t i o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( p r o o f - e r r o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( p r o o f - h i g h l i g h t - d e p e n d e n c y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( p r o o f - h i g h l i g h t - d e p e n d e n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( p r o o f - l o c k e d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )        ` ( p r o o f - m o u s e - h i g h l i g h t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( p r o o f - q u e u e - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( p r o o f - r e g i o n - m o u s e - h i g h l i g h t - f a c e   ( ( t   ( : i n h e r i t   p r o o f - m o u s e - h i g h l i g h t - f a c e ) ) ) )        ` ( p r o o f - s c r i p t - h i g h l i g h t - e r r o r - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 2 ) ) ) )        ` ( p r o o f - t a c t i c a l s - n a m e - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e   : f o r e g r o u n d   n i l   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( p r o o f - t a c t i c s - n a m e - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e   : f o r e g r o u n d   n i l   : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( p r o o f - w a r n i n g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )  ; ; ; ; ;   r a c k e t - m o d e        ` ( r a c k e t - k e y w o r d - a r g u m e n t - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( r a c k e t - s e l f e v a l - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - t y p e - f a c e ) ) ) )  ; ; ; ; ;   r a i n b o w - d e l i m i t e r s        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 1 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 2 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 3 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 4 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 5 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 6 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 7 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 1 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 8 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 9 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 1 0 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 1 1 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( r a i n b o w - d e l i m i t e r s - d e p t h - 1 2 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 5 ) ) ) )  ; ; ; ; ;   r c i r c        ` ( r c i r c - m y - n i c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( r c i r c - o t h e r - n i c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( r c i r c - b r i g h t - n i c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( r c i r c - d i m - n i c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 2 ) ) ) )        ` ( r c i r c - s e r v e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( r c i r c - s e r v e r - p r e f i x   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( r c i r c - t i m e s t a m p   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( r c i r c - n i c k - i n - m e s s a g e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r c i r c - n i c k - i n - m e s s a g e - f u l l - l i n e   ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( r c i r c - p r o m p t   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( r c i r c - t r a c k - n i c k   ( ( t   ( : i n v e r s e - v i d e o   t ) ) ) )        ` ( r c i r c - t r a c k - k e y w o r d   ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( r c i r c - u r l   ( ( t   ( : w e i g h t   b o l d ) ) ) )        ` ( r c i r c - k e y w o r d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   r e - b u i l d e r        ` ( r e b - m a t c h - 0   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( r e b - m a t c h - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( r e b - m a t c h - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( r e b - m a t c h - 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   r e a l g u d        ` ( r e a l g u d - o v e r l a y - a r r o w 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( r e a l g u d - o v e r l a y - a r r o w 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r e a l g u d - o v e r l a y - a r r o w 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( r e a l g u d - b p - e n a b l e d - f a c e   ( ( t   ( : i n h e r i t   e r r o r ) ) ) )        ` ( r e a l g u d - b p - d i s a b l e d - f a c e   ( ( t   ( : i n h e r i t   s e c o n d a r y - s e l e c t i o n ) ) ) )        ` ( r e a l g u d - b p - l i n e - e n a b l e d - f a c e   ( ( t   ( : b o x   ( : c o l o r   , z e n b u r n - r e d   : s t y l e   n i l ) ) ) ) )        ` ( r e a l g u d - b p - l i n e - d i s a b l e d - f a c e   ( ( t   ( : b o x   ( : c o l o r   " g r e y 7 0 "   : s t y l e   n i l ) ) ) ) )        ` ( r e a l g u d - l i n e - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r e a l g u d - b a c k t r a c e - n u m b e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ,   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   r e g e x - t o o l        ` ( r e g e x - t o o l - m a t c h e d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b l u e - 4   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   r p m - m o d e        ` ( r p m - s p e c - d i r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( r p m - s p e c - d o c - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( r p m - s p e c - g h o s t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( r p m - s p e c - m a c r o - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r p m - s p e c - o b s o l e t e - t a g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( r p m - s p e c - p a c k a g e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( r p m - s p e c - s e c t i o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( r p m - s p e c - t a g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( r p m - s p e c - v a r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   r s t - m o d e        ` ( r s t - l e v e l - 1 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( r s t - l e v e l - 2 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( r s t - l e v e l - 3 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( r s t - l e v e l - 4 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2 ) ) ) )        ` ( r s t - l e v e l - 5 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( r s t - l e v e l - 6 - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n - 2 ) ) ) )  ; ; ; ; ;   s h - m o d e        ` ( s h - h e r e d o c           ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( s h - q u o t e d - e x e c   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   s h o w - p a r e n        ` ( s h o w - p a r e n - m i s m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : b a c k g r o u n d   , z e n b u r n - b g + 3   : w e i g h t   b o l d ) ) ) )        ` ( s h o w - p a r e n - m a t c h   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g   : b a c k g r o u n d   , z e n b u r n - b g + 3   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   s m a r t - m o d e - l i n e        ; ;   u s e   ( s e t q   s m l / t h e m e   n i l )   t o   e n a b l e   Z e n b u r n   f o r   s m l        ` ( s m l / g l o b a l   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - f g   : w e i g h t   b o l d ) ) ) )        ` ( s m l / m o d e s   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( s m l / m i n o r - m o d e s   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - f g - 1   : w e i g h t   b o l d ) ) ) )        ` ( s m l / f i l e n a m e   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( s m l / l i n e - n u m b e r   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( s m l / c o l - n u m b e r   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1   : w e i g h t   b o l d ) ) ) )        ` ( s m l / p o s i t i o n - p e r c e n t a g e   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1   : w e i g h t   b o l d ) ) ) )        ` ( s m l / p r e f i x   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( s m l / g i t   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3 ) ) ) )        ` ( s m l / p r o c e s s   ( ( , c l a s s   ( : w e i g h t   b o l d ) ) ) )        ` ( s m l / s u d o   ( ( , c l a s s     ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( s m l / r e a d - o n l y   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - r e d - 2 ) ) ) )        ` ( s m l / o u t s i d e - m o d i f i e d   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( s m l / m o d i f i e d   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( s m l / v c - e d i t e d   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( s m l / c h a r g i n g   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )        ` ( s m l / d i s c h a r g i n g   ( ( , c l a s s   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )  ; ; ; ; ;   s m a r t p a r e n s        ` ( s p - s h o w - p a i r - m i s m a t c h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1   : b a c k g r o u n d   , z e n b u r n - b g + 3   : w e i g h t   b o l d ) ) ) )        ` ( s p - s h o w - p a i r - m a t c h - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 3   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   s m l - m o d e - l i n e        ' ( s m l - m o d e l i n e - e n d - f a c e   ( ( t   : i n h e r i t   d e f a u l t   : w i d t h   c o n d e n s e d ) ) )  ; ; ; ; ;   S L I M E        ` ( s l i m e - r e p l - o u t p u t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( s l i m e - r e p l - i n p u t e d - o u t p u t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( s l i m e - e r r o r - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - r e d ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - r e d ) ) ) )        ` ( s l i m e - w a r n i n g - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - o r a n g e ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - o r a n g e ) ) ) )        ` ( s l i m e - s t y l e - w a r n i n g - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - y e l l o w ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - y e l l o w ) ) ) )        ` ( s l i m e - n o t e - f a c e            ( ( ( ( s u p p o r t s   : u n d e r l i n e   ( : s t y l e   w a v e ) ) )                ( : u n d e r l i n e   ( : s t y l e   w a v e   : c o l o r   , z e n b u r n - g r e e n ) ) )              ( t                ( : u n d e r l i n e   , z e n b u r n - g r e e n ) ) ) )        ` ( s l i m e - h i g h l i g h t - f a c e   ( ( t   ( : i n h e r i t   h i g h l i g h t ) ) ) )  ; ; ; ; ;   s p e e d b a r        ` ( s p e e d b a r - b u t t o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( s p e e d b a r - d i r e c t o r y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( s p e e d b a r - f i l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( s p e e d b a r - h i g h l i g h t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( s p e e d b a r - s e l e c t e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( s p e e d b a r - s e p a r a t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - b l u e - 1 ) ) ) )        ` ( s p e e d b a r - t a g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )  ; ; ; ; ;   s x        ` ( s x - c u s t o m - b u t t o n            ( ( t   ( : b a c k g r o u n d   , z e n b u r n - f g   : f o r e g r o u n d   , z e n b u r n - b g - 1                      : b o x   ( : l i n e - w i d t h   3   : s t y l e   r e l e a s e d - b u t t o n )   : h e i g h t   0 . 9 ) ) ) )        ` ( s x - q u e s t i o n - l i s t - a n s w e r s            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3                      : h e i g h t   1 . 0   : i n h e r i t   s x - q u e s t i o n - l i s t - p a r e n t ) ) ) )        ` ( s x - q u e s t i o n - m o d e - a c c e p t e d            ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3                      : h e i g h t   1 . 3   : i n h e r i t   s x - q u e s t i o n - m o d e - t i t l e ) ) ) )        ' ( s x - q u e s t i o n - m o d e - c o n t e n t - f a c e   ( ( t   ( : i n h e r i t   h i g h l i g h t ) ) ) )        ` ( s x - q u e s t i o n - m o d e - k b d - t a g            ( ( t   ( : b o x   ( : c o l o r   , z e n b u r n - b g - 1   : l i n e - w i d t h   3   : s t y l e   r e l e a s e d - b u t t o n )                      : h e i g h t   0 . 9   : w e i g h t   s e m i - b o l d ) ) ) )  ; ; ; ; ;   t a b b a r        ` ( t a b b a r - b u t t o n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g                                                                          : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( t a b b a r - s e l e c t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g                                                                              : b a c k g r o u n d   , z e n b u r n - b g                                                                              : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   p r e s s e d - b u t t o n ) ) ) ) )        ` ( t a b b a r - u n s e l e c t e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g                                                                                  : b a c k g r o u n d   , z e n b u r n - b g + 1                                                                                  : b o x   ( : l i n e - w i d t h   - 1   : s t y l e   r e l e a s e d - b u t t o n ) ) ) ) )  ; ; ; ; ;   t e r m        ` ( t e r m - c o l o r - b l a c k   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g                                                                                : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( t e r m - c o l o r - r e d   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 2                                                                            : b a c k g r o u n d   , z e n b u r n - r e d - 4 ) ) ) )        ` ( t e r m - c o l o r - g r e e n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n                                                                                : b a c k g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( t e r m - c o l o r - y e l l o w   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e                                                                                  : b a c k g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( t e r m - c o l o r - b l u e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e - 1                                                                              : b a c k g r o u n d   , z e n b u r n - b l u e - 4 ) ) ) )        ` ( t e r m - c o l o r - m a g e n t a   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a                                                                                    : b a c k g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( t e r m - c o l o r - c y a n   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n                                                                              : b a c k g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( t e r m - c o l o r - w h i t e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g                                                                                : b a c k g r o u n d   , z e n b u r n - f g - 1 ) ) ) )        ' ( t e r m - d e f a u l t - f g - c o l o r   ( ( t   ( : i n h e r i t   t e r m - c o l o r - w h i t e ) ) ) )        ' ( t e r m - d e f a u l t - b g - c o l o r   ( ( t   ( : i n h e r i t   t e r m - c o l o r - b l a c k ) ) ) )  ; ; ; ; ;   u n d o - t r e e        ` ( u n d o - t r e e - v i s u a l i z e r - a c t i v e - b r a n c h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g + 1   : w e i g h t   b o l d ) ) ) )        ` ( u n d o - t r e e - v i s u a l i z e r - c u r r e n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1   : w e i g h t   b o l d ) ) ) )        ` ( u n d o - t r e e - v i s u a l i z e r - d e f a u l t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( u n d o - t r e e - v i s u a l i z e r - r e g i s t e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( u n d o - t r e e - v i s u a l i z e r - u n m o d i f i e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )  ; ; ; ; ;   v i s u a l - r e g e x p        ` ( v r / g r o u p - 0   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - g r e e n   : w e i g h t   b o l d ) ) ) )        ` ( v r / g r o u p - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - o r a n g e   : w e i g h t   b o l d ) ) ) )        ` ( v r / g r o u p - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - b l u e   : w e i g h t   b o l d ) ) ) )        ` ( v r / m a t c h - 0   ( ( t   ( : i n h e r i t   i s e a r c h ) ) ) )        ` ( v r / m a t c h - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w - 2   : b a c k g r o u n d   , z e n b u r n - b g - 1   : w e i g h t   b o l d ) ) ) )        ` ( v r / m a t c h - s e p a r a t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   v o l a t i l e - h i g h l i g h t s        ` ( v h l / d e f a u l t - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 0 5 ) ) ) )  ; ; ; ; ;   w e b - m o d e        ` ( w e b - m o d e - b u i l t i n - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - b u i l t i n - f a c e ) ) ) )        ` ( w e b - m o d e - c o m m e n t - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( w e b - m o d e - c o n s t a n t - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( w e b - m o d e - c s s - a t - r u l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e   ) ) ) )        ` ( w e b - m o d e - c s s - p r o p - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( w e b - m o d e - c s s - p s e u d o - c l a s s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 3   : w e i g h t   b o l d ) ) ) )        ` ( w e b - m o d e - c s s - r u l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w e b - m o d e - d o c t y p e - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - c o m m e n t - f a c e ) ) ) )        ` ( w e b - m o d e - f o l d e d - f a c e   ( ( t   ( : u n d e r l i n e   t ) ) ) )        ` ( w e b - m o d e - f u n c t i o n - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w e b - m o d e - h t m l - a t t r - n a m e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( w e b - m o d e - h t m l - a t t r - v a l u e - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - s t r i n g - f a c e ) ) ) )        ` ( w e b - m o d e - h t m l - t a g - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n ) ) ) )        ` ( w e b - m o d e - k e y w o r d - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - k e y w o r d - f a c e ) ) ) )        ` ( w e b - m o d e - p r e p r o c e s s o r - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - p r e p r o c e s s o r - f a c e ) ) ) )        ` ( w e b - m o d e - s t r i n g - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - s t r i n g - f a c e ) ) ) )        ` ( w e b - m o d e - t y p e - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - t y p e - f a c e ) ) ) )        ` ( w e b - m o d e - v a r i a b l e - n a m e - f a c e   ( ( t   ( : i n h e r i t   , f o n t - l o c k - v a r i a b l e - n a m e - f a c e ) ) ) )        ` ( w e b - m o d e - s e r v e r - b a c k g r o u n d - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g ) ) ) )        ` ( w e b - m o d e - s e r v e r - c o m m e n t - f a c e   ( ( t   ( : i n h e r i t   w e b - m o d e - c o m m e n t - f a c e ) ) ) )        ` ( w e b - m o d e - s e r v e r - s t r i n g - f a c e   ( ( t   ( : i n h e r i t   w e b - m o d e - s t r i n g - f a c e ) ) ) )        ` ( w e b - m o d e - s y m b o l - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - c o n s t a n t - f a c e ) ) ) )        ` ( w e b - m o d e - w a r n i n g - f a c e   ( ( t   ( : i n h e r i t   f o n t - l o c k - w a r n i n g - f a c e ) ) ) )        ` ( w e b - m o d e - w h i t e s p a c e s - f a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   w h i t e s p a c e - m o d e        ` ( w h i t e s p a c e - s p a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1   : f o r e g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( w h i t e s p a c e - h s p a c e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g + 1   : f o r e g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( w h i t e s p a c e - t a b   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( w h i t e s p a c e - n e w l i n e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g + 1 ) ) ) )        ` ( w h i t e s p a c e - t r a i l i n g   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( w h i t e s p a c e - l i n e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g   : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( w h i t e s p a c e - s p a c e - b e f o r e - t a b   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - o r a n g e   : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( w h i t e s p a c e - i n d e n t a t i o n   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w   : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( w h i t e s p a c e - e m p t y   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( w h i t e s p a c e - s p a c e - a f t e r - t a b   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - y e l l o w   : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )  ; ; ; ; ;   w a n d e r l u s t        ` ( w l - h i g h l i g h t - f o l d e r - f e w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 2 ) ) ) )        ` ( w l - h i g h l i g h t - f o l d e r - m a n y - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( w l - h i g h l i g h t - f o l d e r - p a t h - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - o r a n g e ) ) ) )        ` ( w l - h i g h l i g h t - f o l d e r - u n r e a d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w l - h i g h l i g h t - f o l d e r - z e r o - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( w l - h i g h l i g h t - f o l d e r - u n k n o w n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - c i t a t i o n - h e a d e r   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d - 1 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - c i t e d - t e x t - 1   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - c i t e d - t e x t - 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - c i t e d - t e x t - 3   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - c i t e d - t e x t - 4   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - h e a d e r - c o n t e n t s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - h e a d e r s - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d + 1 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - i m p o r t a n t - h e a d e r - c o n t e n t s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - h e a d e r - c o n t e n t s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 1 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - i m p o r t a n t - h e a d e r - c o n t e n t s 2   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 2 ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - s i g n a t u r e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n ) ) ) )        ` ( w l - h i g h l i g h t - m e s s a g e - u n i m p o r t a n t - h e a d e r - c o n t e n t s   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - a n s w e r e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - d i s p o s e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g                                                                                                                    : s l a n t   i t a l i c ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - n e w - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b l u e ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - n o r m a l - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - t h r e a d - t o p - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w ) ) ) )        ` ( w l - h i g h l i g h t - t h r e a d - i n d e n t - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - m a g e n t a ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - r e f i l e d - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - f g ) ) ) )        ` ( w l - h i g h l i g h t - s u m m a r y - d i s p l a y i n g - f a c e   ( ( t   ( : u n d e r l i n e   t   : w e i g h t   b o l d ) ) ) )  ; ; ; ; ;   w h i c h - f u n c - m o d e        ` ( w h i c h - f u n c   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - g r e e n + 4 ) ) ) )  ; ; ; ; ;   x c s c o p e        ` ( c s c o p e - f i l e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - y e l l o w   : w e i g h t   b o l d ) ) ) )        ` ( c s c o p e - f u n c t i o n - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - c y a n   : w e i g h t   b o l d ) ) ) )        ` ( c s c o p e - l i n e - n u m b e r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d ) ) ) )        ` ( c s c o p e - m o u s e - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - b g   : b a c k g r o u n d   , z e n b u r n - b l u e + 1 ) ) ) )        ` ( c s c o p e - s e p a r a t o r - f a c e   ( ( t   ( : f o r e g r o u n d   , z e n b u r n - r e d   : w e i g h t   b o l d                                                                                          : u n d e r l i n e   t   : o v e r l i n e   t ) ) ) )  ; ; ; ; ;   y a s c r o l l        ` ( y a s c r o l l : t h u m b - t e x t - a r e a   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ` ( y a s c r o l l : t h u m b - f r i n g e   ( ( t   ( : b a c k g r o u n d   , z e n b u r n - b g - 1   : f o r e g r o u n d   , z e n b u r n - b g - 1 ) ) ) )        ) )   ; ; ;   T h e m e   V a r i a b l e s  ( z e n b u r n - w i t h - c o l o r - v a r i a b l e s      ( c u s t o m - t h e m e - s e t - v a r i a b l e s        ' z e n b u r n  ; ; ; ; ;   a n s i - c o l o r        ` ( a n s i - c o l o r - n a m e s - v e c t o r   [ , z e n b u r n - b g   , z e n b u r n - r e d   , z e n b u r n - g r e e n   , z e n b u r n - y e l l o w                                                                                      , z e n b u r n - b l u e   , z e n b u r n - m a g e n t a   , z e n b u r n - c y a n   , z e n b u r n - f g ] )  ; ; ; ; ;   c o m p a n y - q u i c k h e l p        ` ( c o m p a n y - q u i c k h e l p - c o l o r - b a c k g r o u n d   , z e n b u r n - b g + 1 )        ` ( c o m p a n y - q u i c k h e l p - c o l o r - f o r e g r o u n d   , z e n b u r n - f g )  ; ; ; ; ;   f i l l - c o l u m n - i n d i c a t o r        ` ( f c i - r u l e - c o l o r   , z e n b u r n - b g - 0 5 )  ; ; ; ; ;   n r e p l - c l i e n t        ` ( n r e p l - m e s s a g e - c o l o r s            ' ( , z e n b u r n - r e d   , z e n b u r n - o r a n g e   , z e n b u r n - y e l l o w   , z e n b u r n - g r e e n   , z e n b u r n - g r e e n + 4                , z e n b u r n - c y a n   , z e n b u r n - b l u e + 1   , z e n b u r n - m a g e n t a ) )  ; ; ; ; ;   p d f - t o o l s        ` ( p d f - v i e w - m i d n i g h t - c o l o r s   ' ( , z e n b u r n - f g   .   , z e n b u r n - b g - 0 5 ) )  ; ; ; ; ;   v c - a n n o t a t e        ` ( v c - a n n o t a t e - c o l o r - m a p            ' ( (   2 0 .   .   , z e n b u r n - r e d - 1 )                (   4 0 .   .   , z e n b u r n - r e d )                (   6 0 .   .   , z e n b u r n - o r a n g e )                (   8 0 .   .   , z e n b u r n - y e l l o w - 2 )                ( 1 0 0 .   .   , z e n b u r n - y e l l o w - 1 )                ( 1 2 0 .   .   , z e n b u r n - y e l l o w )                ( 1 4 0 .   .   , z e n b u r n - g r e e n - 2 )                ( 1 6 0 .   .   , z e n b u r n - g r e e n )                ( 1 8 0 .   .   , z e n b u r n - g r e e n + 1 )                ( 2 0 0 .   .   , z e n b u r n - g r e e n + 2 )                ( 2 2 0 .   .   , z e n b u r n - g r e e n + 3 )                ( 2 4 0 .   .   , z e n b u r n - g r e e n + 4 )                ( 2 6 0 .   .   , z e n b u r n - c y a n )                ( 2 8 0 .   .   , z e n b u r n - b l u e - 2 )                ( 3 0 0 .   .   , z e n b u r n - b l u e - 1 )                ( 3 2 0 .   .   , z e n b u r n - b l u e )                ( 3 4 0 .   .   , z e n b u r n - b l u e + 1 )                ( 3 6 0 .   .   , z e n b u r n - m a g e n t a ) ) )        ` ( v c - a n n o t a t e - v e r y - o l d - c o l o r   , z e n b u r n - m a g e n t a )        ` ( v c - a n n o t a t e - b a c k g r o u n d   , z e n b u r n - b g - 1 )        ) )   ; ; ;   R a i n b o w   S u p p o r t   ( d e c l a r e - f u n c t i o n   r a i n b o w - m o d e   ' r a i n b o w - m o d e )  ( d e c l a r e - f u n c t i o n   r a i n b o w - c o l o r i z e - b y - a s s o c   ' r a i n b o w - m o d e )   ( d e f v a r   z e n b u r n - a d d - f o n t - l o c k - k e y w o r d s   n i l      " W h e t h e r   t o   a d d   f o n t - l o c k   k e y w o r d s   f o r   z e n b u r n   c o l o r   n a m e s .  I n   b u f f e r s   v i s i t i n g   l i b r a r y   ` z e n b u r n - t h e m e . e l '   t h e   z e n b u r n  s p e c i f i c   k e y w o r d s   a r e   a l w a y s   a d d e d .     I n   a l l   o t h e r   E m a c s - L i s p  b u f f e r s   t h i s   v a r i a b l e   c o n t r o l s   w h e t h e r   t h i s   s h o u l d   b e   d o n e .  T h i s   r e q u i r e s   l i b r a r y   ` r a i n b o w - m o d e ' . " )   ( d e f v a r   z e n b u r n - c o l o r s - f o n t - l o c k - k e y w o r d s   n i l )   ; ;   ( d e f a d v i c e   r a i n b o w - t u r n - o n   ( a f t e r   z e n b u r n   a c t i v a t e )  ; ;       " M a y b e   a l s o   a d d   f o n t - l o c k   k e y w o r d s   f o r   z e n b u r n   c o l o r s . "  ; ;       ( w h e n   ( a n d   ( d e r i v e d - m o d e - p   ' e m a c s - l i s p - m o d e )  ; ;                             ( o r   z e n b u r n - a d d - f o n t - l o c k - k e y w o r d s  ; ;                                     ( e q u a l   ( f i l e - n a m e - n o n d i r e c t o r y   ( b u f f e r - f i l e - n a m e ) )  ; ;                                                   " z e n b u r n - t h e m e . e l " ) ) )  ; ;           ( u n l e s s   z e n b u r n - c o l o r s - f o n t - l o c k - k e y w o r d s  ; ;               ( s e t q   z e n b u r n - c o l o r s - f o n t - l o c k - k e y w o r d s  ; ;                           ` ( ( , ( r e g e x p - o p t   ( m a p c a r   ' c a r   z e n b u r n - c o l o r s - a l i s t )   ' w o r d s )  ; ;                                 ( 0   ( r a i n b o w - c o l o r i z e - b y - a s s o c   z e n b u r n - c o l o r s - a l i s t ) ) ) ) ) )  ; ;           ( f o n t - l o c k - a d d - k e y w o r d s   n i l   z e n b u r n - c o l o r s - f o n t - l o c k - k e y w o r d s ) ) )   ; ;   ( d e f a d v i c e   r a i n b o w - t u r n - o f f   ( a f t e r   z e n b u r n   a c t i v a t e )  ; ;       " A l s o   r e m o v e   f o n t - l o c k   k e y w o r d s   f o r   z e n b u r n   c o l o r s . "  ; ;       ( f o n t - l o c k - r e m o v e - k e y w o r d s   n i l   z e n b u r n - c o l o r s - f o n t - l o c k - k e y w o r d s ) )   ; ; ;   F o o t e r   ; ; ; # # # a u t o l o a d  ( a n d   l o a d - f i l e - n a m e            ( b o u n d p   ' c u s t o m - t h e m e - l o a d - p a t h )            ( a d d - t o - l i s t   ' c u s t o m - t h e m e - l o a d - p a t h                                      ( f i l e - n a m e - a s - d i r e c t o r y                                        ( f i l e - n a m e - d i r e c t o r y   l o a d - f i l e - n a m e ) ) ) )   ( p r o v i d e - t h e m e   ' z e n b u r n )   ; ;   L o c a l   V a r i a b l e s :  ; ;   n o - b y t e - c o m p i l e :   t  ; ;   i n d e n t - t a b s - m o d e :   n i l  ; ;   e v a l :   ( w h e n   ( r e q u i r e   ' r a i n b o w - m o d e   n i l   t )   ( r a i n b o w - m o d e   1 ) )  ; ;   E n d :  ; ; ;   z e n b u r n - t h e m e . e l   e n d s   h e r e _ l�;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2018 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.7-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburn "The Zenburn color theme")

(defgroup zenburn-theme nil
  "Zenburn theme."
  :prefix "zenburn-theme-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

;;;###autoload
(defcustom zenburn-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defcustom zenburn-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

;;; Color Palette

(defvar zenburn-default-colors-alist
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburn-default-colors-alist
                           zenburn-override-colors-alist))
         (z-variable-pitch (if zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
   `(widget-field ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
   `(escape-glyph ((t (:foreground ,zenburn-yellow :weight bold))))
   `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(header-line ((t (:foreground ,zenburn-yellow
                                  :background ,zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,zenburn-bg-05))))
   `(success ((t (:foreground ,zenburn-green :weight bold))))
   `(warning ((t (:foreground ,zenburn-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,zenburn-blue))))
   `(compilation-info ((t (:foreground ,zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-line-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburn-fg-1))))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburn-fg))))
   `(grep-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburn-blue))))
   `(grep-match-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(match ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))
   `(hi-green   ((t (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))
   `(hi-pink    ((t (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))
   `(hi-yellow  ((t (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenburn-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenburn-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

   `(menu ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburn-green+1
                           :background ,zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburn-green-2
                      :background ,zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburn-red))))
   `(vertical-border ((t (:foreground ,zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-2))))
   `(font-lock-constant-face ((t (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenburn-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,zenburn-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-green))))
   `(anzu-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(anzu-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburn-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburn-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenburn-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenburn-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenburn-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburn-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburn-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenburn-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenburn-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenburn-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
   `(popup-tip-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-summary-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenburn-green+3 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburn-orange :background ,zenburn-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburn-bg+2))))
   `(company-preview ((t (:background ,zenburn-green+2))))
   `(company-preview-common ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
   `(bm-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,zenburn-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,zenburn-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,zenburn-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,zenburn-cyan))))
   `(cfw:face-saturday ((t (:foreground ,zenburn-blue :weight bold))))
   `(cfw:face-select ((t (:background ,zenburn-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,zenburn-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,zenburn-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,zenburn-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenburn-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburn-green+1))))
   `(cider-deprecated-face ((t (:background ,zenburn-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenburn-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenburn-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenburn-red-4))))
   `(cider-test-error-face ((t (:background ,zenburn-magenta))))
   `(cider-test-success-face ((t (:background ,zenburn-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,zenburn-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenburn-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenburn-fg))))
   `(circe-fool-face ((t (:foreground ,zenburn-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenburn-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenburn-fg))))
   `(circe-server-face ((t (:foreground ,zenburn-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenburn-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenburn-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenburn-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenburn-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenburn-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenburn-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenburn-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenburn-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenburn-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenburn-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenburn-blue :foreground ,zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,zenburn-bg-05 :foreground ,zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,zenburn-cyan :foreground ,zenburn-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,zenburn-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,zenburn-green))))
   `(debbugs-gnu-new ((t (:foreground ,zenburn-red))))
   `(debbugs-gnu-pending ((t (:foreground ,zenburn-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,zenburn-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,zenburn-red))))
;;;;; diff
   `(diff-added          ((t (:background ,zenburn-green-5 :foreground ,zenburn-green+2))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburn-yellow-1))))
   `(diff-removed        ((t (:background ,zenburn-red-6 :foreground ,zenburn-red+1))))
   `(diff-refine-added   ((t (:background ,zenburn-green-4 :foreground ,zenburn-green+3))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburn-yellow))))
   `(diff-refine-removed ((t (:background ,zenburn-red-5 :foreground ,zenburn-red+2))))
   `(diff-header ((,class (:background ,zenburn-bg+2))
                  (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))
      (t (:background ,zenburn-fg :foreground ,zenburn-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburn-blue :background ,zenburn-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburn-red+1 :background ,zenburn-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburn-green+1 :background ,zenburn-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,zenburn-fg))))
   `(diredp-number ((t (:foreground ,zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenburn-green-2))))
   `(diredp-symlink ((t (:foreground ,zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenburn-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenburn-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenburn-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenburn-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,zenburn-orange))))
   `(diredfl-date-time ((t (:foreground ,zenburn-magenta))))
   `(diredfl-deletion ((t (:foreground ,zenburn-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,zenburn-red))))
   `(diredfl-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,zenburn-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,zenburn-red))))
   `(diredfl-executable-tag ((t (:foreground ,zenburn-green+1))))
   `(diredfl-file-name ((t (:foreground ,zenburn-blue))))
   `(diredfl-file-suffix ((t (:foreground ,zenburn-green))))
   `(diredfl-flag-mark ((t (:foreground ,zenburn-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,zenburn-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,zenburn-red))))
   `(diredfl-link-priv ((t (:foreground ,zenburn-yellow))))
   `(diredfl-no-priv ((t (:foreground ,zenburn-fg))))
   `(diredfl-number ((t (:foreground ,zenburn-green+1))))
   `(diredfl-other-priv ((t (:foreground ,zenburn-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,zenburn-red-1))))
   `(diredfl-read-priv ((t (:foreground ,zenburn-green-1))))
   `(diredfl-symlink ((t (:foreground ,zenburn-yellow))))
   `(diredfl-write-priv ((t (:foreground ,zenburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:inherit ediff-current-diff-A))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,zenburn-blue+2 :background ,zenburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenburn-bg+1))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:inherit ediff-fine-diff-A))))
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenburn-blue+3 :background ,zenburn-blue-4 :weight bold))))
   `(ediff-odd-diff-A ((t (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenburn-green+3))))
   `(egg-branch ((t (:foreground ,zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenburn-yellow))))
   `(egg-term ((t (:foreground ,zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenburn-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenburn-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenburn-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenburn-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenburn-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenburn-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenburn-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,zenburn-bg-1
                                     :foreground ,zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenburn-green))))
   `(erc-pal-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))
      (t (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))
      (t (:foreground ,zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenburn-fg))))
   `(ack-file ((t (:foreground ,zenburn-blue))))
   `(ack-line ((t (:foreground ,zenburn-yellow))))
   `(ack-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenburn-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenburn-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenburn-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenburn-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenburn-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,zenburn-yellow))))
   `(gnus-x ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(mm-uu-extract ((t (:background ,zenburn-bg-05 :foreground ,zenburn-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,zenburn-bg-1 :background ,zenburn-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenburn-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenburn-green
                      :background ,zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,zenburn-yellow
                      :background ,zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburn-orange :background ,zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenburn-red-1 :background ,zenburn-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenburn-red-3 :background ,zenburn-bg))))
   `(hydra-face-blue ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(hydra-face-pink ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(hydra-face-teal ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(info-constant-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-function-ref-item ((t (:background ,zenburn-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-menu ((t (:foreground ,zenburn-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,zenburn-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue+1))))
   `(info-user-option-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-red))))
   `(info-variable-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,zenburn-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,zenburn-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,zenburn-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,zenburn-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,zenburn-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,zenburn-green+3))))
   `(irfc-title-face ((t (:foreground ,zenburn-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenburn-green :background ,zenburn-bg))))
   `(ivy-current-match ((t (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(ivy-match-required-face ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-green+1))))
   `(ivy-remote ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(ivy-subdir ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenburn-yellow))))
   `(ido-indicator ((t (:foreground ,zenburn-yellow :background ,zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenburn-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenburn-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenburn-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburn-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenburn-green+3))))
   `(jabber-activity-face((t (:foreground ,zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenburn-orange))))
   `(js2-error ((t (:foreground ,zenburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,zenburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenburn-green+3))))
   `(js2-function-param ((t (:foreground, zenburn-orange))))
   `(js2-external-variable ((t (:foreground ,zenburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburn-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburn-red-1))))
   `(js2-object-property ((t (:foreground ,zenburn-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenburn-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenburn-cyan))))
   `(js2-function-call ((t (:foreground ,zenburn-cyan))))
   `(js2-private-member ((t (:foreground ,zenburn-blue-1))))
   `(js2-keywords ((t (:foreground ,zenburn-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,zenburn-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenburn-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,zenburn-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,zenburn-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,zenburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenburn-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenburn-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburn-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburn-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburn-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenburn-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenburn-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenburn-magenta :background ,zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburn-orange :weight bold))))

   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburn-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburn-bg+05
                                                        :foreground ,zenburn-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburn-bg+2
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburn-orange
                                                        :foreground ,zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,zenburn-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburn-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-date    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-summary ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburn-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenburn-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenburn-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburn-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,zenburn-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,zenburn-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,zenburn-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,zenburn-fg+1))))
   `(markup-meta-face ((t (:foreground ,zenburn-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,zenburn-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,zenburn-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,zenburn-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburn-green+1))))
   `(message-header-other ((t (:foreground ,zenburn-green))))
   `(message-header-to ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburn-green))))
   `(message-mml ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenburn-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,zenburn-magenta))))
   `(mingus-song-file-face ((t (:foreground ,zenburn-yellow))))
   `(mingus-artist-face ((t (:foreground ,zenburn-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,zenburn-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,zenburn-red+1))))
   `(mingus-stopped-face ((t (:foreground ,zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,zenburn-red))))
   `(nav-face-file ((t (:foreground ,zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,zenburn-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))
      (t
       (:underline ,zenburn-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))
      (t
       (:underline ,zenburn-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenburn-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenburn-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenburn-blue))))
   `(neo-file-link-face ((t (:foreground ,zenburn-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenburn-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenburn-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenburn-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenburn-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenburn-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburn-red :background ,zenburn-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenburn-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenburn-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenburn-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,zenburn-green+3))))
   `(org-formula ((t (:foreground ,zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenburn-green+3))))
   `(org-hide ((t (:foreground ,zenburn-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
   `(org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenburn-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,zenburn-orange))))
   `(org-todo ((t (:weight bold :foreground ,zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenburn-bg-1))))
   `(org-column-title ((t (:background ,zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenburn-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue
                                         :weight bold :height ,zenburn-height-plus-4))))
   `(org-document-info ((t (:foreground ,zenburn-blue))))
   `(org-habit-ready-face ((t :background ,zenburn-green)))
   `(org-habit-alert-face ((t :background ,zenburn-yellow-1 :foreground ,zenburn-bg)))
   `(org-habit-clear-face ((t :background ,zenburn-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenburn-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenburn-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenburn-green-2)))
   `(org-habit-alert-future-face ((t :background ,zenburn-yellow-2 :foreground ,zenburn-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenburn-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,zenburn-magenta))))
   `(cperl-array-face ((t (:foreground ,zenburn-yellow, :backgorund ,zenburn-bg))))
   `(cperl-hash-face ((t (:foreground ,zenburn-yellow-1, :background ,zenburn-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,zenburn-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-error-face ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-locked-face ((t (:background ,zenburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-queue-face ((t (:background ,zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-warning-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,zenburn-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,zenburn-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(reb-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(reb-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(reb-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,zenburn-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,zenburn-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,zenburn-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenburn-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,zenburn-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,zenburn-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,zenburn-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenburn-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenburn-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburn for sml
   `(sml/global ((,class (:foreground ,zenburn-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburn-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburn-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburn-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburn-orange))))
   `(sml/git ((,class (:foreground ,zenburn-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburn-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburn-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburn-orange))))
   `(sml/modified ((,class (:foreground ,zenburn-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburn-green+2))))
   `(sml/charging ((,class (:foreground ,zenburn-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburn-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))
      (t
       (:underline ,zenburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))
      (t
       (:underline ,zenburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow)))
      (t
       (:underline ,zenburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)))
      (t
       (:underline ,zenburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenburn-red))))
   `(speedbar-separator-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenburn-yellow))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,zenburn-fg :foreground ,zenburn-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,zenburn-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,zenburn-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,zenburn-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenburn-fg
                                    :background ,zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,zenburn-fg
                                      :background ,zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenburn-fg
                                        :background ,zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburn-bg
                                       :background ,zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,zenburn-red-2
                                     :background ,zenburn-red-4))))
   `(term-color-green ((t (:foreground ,zenburn-green
                                       :background ,zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburn-orange
                                        :background ,zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,zenburn-blue-1
                                      :background ,zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburn-magenta
                                         :background ,zenburn-red))))
   `(term-color-cyan ((t (:foreground ,zenburn-cyan
                                      :background ,zenburn-blue))))
   `(term-color-white ((t (:foreground ,zenburn-fg
                                       :background ,zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburn-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,zenburn-bg :background ,zenburn-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,zenburn-yellow-2 :background ,zenburn-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,zenburn-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburn-red))))
   `(whitespace-line ((t (:background ,zenburn-bg :foreground ,zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburn-orange :foreground ,zenburn-orange))))
   `(whitespace-indentation ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
   `(whitespace-empty ((t (:background ,zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenburn-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenburn-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenburn-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenburn-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
   ))

;;; Theme Variables
(zenburn-with-color-variables
  (custom-theme-set-variables
   'zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow
                                          ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenburn-bg+1)
   `(company-quickhelp-color-foreground ,zenburn-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburn-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburn-red ,zenburn-orange ,zenburn-yellow ,zenburn-green ,zenburn-green+4
       ,zenburn-cyan ,zenburn-blue+1 ,zenburn-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburn-fg . ,zenburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburn-red-1)
       ( 40. . ,zenburn-red)
       ( 60. . ,zenburn-orange)
       ( 80. . ,zenburn-yellow-2)
       (100. . ,zenburn-yellow-1)
       (120. . ,zenburn-yellow)
       (140. . ,zenburn-green-2)
       (160. . ,zenburn-green)
       (180. . ,zenburn-green+1)
       (200. . ,zenburn-green+2)
       (220. . ,zenburn-green+3)
       (240. . ,zenburn-green+4)
       (260. . ,zenburn-cyan)
       (280. . ,zenburn-blue-2)
       (300. . ,zenburn-blue-1)
       (320. . ,zenburn-blue)
       (340. . ,zenburn-blue+1)
       (360. . ,zenburn-magenta)))
   `(vc-annotate-very-old-color ,zenburn-magenta)
   `(vc-annotate-background ,zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburn color names.
In buffers visiting library `zenburn-theme.el' the zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar zenburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after zenburn activate)
;;   "Maybe also add font-lock keywords for zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "zenburn-theme.el")))
;;     (unless zenburn-colors-font-lock-keywords
;;       (setq zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after zenburn activate)
;;   "Also remove font-lock keywords for zenburn colors."
;;   (font-lock-remove-keywords nil zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburn-theme.el ends hereO n�<meta http-equiv="Content-Type" content="text/html;charset=UTF-8"><pre style="color: rgb(0, 0, 0); font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-weight: 400; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; text-decoration-style: initial; text-decoration-color: initial; overflow-wrap: break-word; white-space: pre-wrap;">;;; zenburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2018 Bozhidar Batsov

;; Author: Bozhidar Batsov &lt;bozhidar@batsov.com&gt;
;; URL: http://github.com/bbatsov/zenburn-emacs
;; Version: 2.7-snapshot

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see &lt;http://www.gnu.org/licenses/&gt;.

;;; Commentary:

;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme zenburn "The Zenburn color theme")

(defgroup zenburn-theme nil
  "Zenburn theme."
  :prefix "zenburn-theme-"
  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")
  :tag "Zenburn theme")

;;;###autoload
(defcustom zenburn-override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'zenburn-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defcustom zenburn-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

(defcustom zenburn-scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group 'zenburn-theme
  :package-version '(zenburn . "2.6"))

;;; Color Palette

(defvar zenburn-default-colors-alist
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#656555")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#2B2B2B")
    ("zenburn-bg-05"    . "#383838")
    ("zenburn-bg"       . "#3F3F3F")
    ("zenburn-bg+05"    . "#494949")
    ("zenburn-bg+1"     . "#4F4F4F")
    ("zenburn-bg+2"     . "#5F5F5F")
    ("zenburn-bg+3"     . "#6F6F6F")
    ("zenburn-red+2"    . "#ECB3B3")
    ("zenburn-red+1"    . "#DCA3A3")
    ("zenburn-red"      . "#CC9393")
    ("zenburn-red-1"    . "#BC8383")
    ("zenburn-red-2"    . "#AC7373")
    ("zenburn-red-3"    . "#9C6363")
    ("zenburn-red-4"    . "#8C5353")
    ("zenburn-red-5"    . "#7C4343")
    ("zenburn-red-6"    . "#6C3333")
    ("zenburn-orange"   . "#DFAF8F")
    ("zenburn-yellow"   . "#F0DFAF")
    ("zenburn-yellow-1" . "#E0CF9F")
    ("zenburn-yellow-2" . "#D0BF8F")
    ("zenburn-green-5"  . "#2F4F2F")
    ("zenburn-green-4"  . "#3F5F3F")
    ("zenburn-green-3"  . "#4F6F4F")
    ("zenburn-green-2"  . "#5F7F5F")
    ("zenburn-green-1"  . "#6F8F6F")
    ("zenburn-green"    . "#7F9F7F")
    ("zenburn-green+1"  . "#8FB28F")
    ("zenburn-green+2"  . "#9FC59F")
    ("zenburn-green+3"  . "#AFD8AF")
    ("zenburn-green+4"  . "#BFEBBF")
    ("zenburn-cyan"     . "#93E0E3")
    ("zenburn-blue+3"   . "#BDE0F3")
    ("zenburn-blue+2"   . "#ACE0E3")
    ("zenburn-blue+1"   . "#94BFF3")
    ("zenburn-blue"     . "#8CD0D3")
    ("zenburn-blue-1"   . "#7CB8BB")
    ("zenburn-blue-2"   . "#6CA0A3")
    ("zenburn-blue-3"   . "#5C888B")
    ("zenburn-blue-4"   . "#4C7073")
    ("zenburn-blue-5"   . "#366060")
    ("zenburn-magenta"  . "#DC8CC3"))
  "List of Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro zenburn-with-color-variables (&amp;rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append zenburn-default-colors-alist
                           zenburn-override-colors-alist))
         (z-variable-pitch (if zenburn-use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg+1))))
   `(widget-field ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3))))
   `(escape-glyph ((t (:foreground ,zenburn-yellow :weight bold))))
   `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(header-line ((t (:foreground ,zenburn-yellow
                                  :background ,zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,zenburn-bg-05))))
   `(success ((t (:foreground ,zenburn-green :weight bold))))
   `(warning ((t (:foreground ,zenburn-orange :weight bold))))
   `(tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,zenburn-blue))))
   `(compilation-info ((t (:foreground ,zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,zenburn-green))))
   `(compilation-line-face ((t (:foreground ,zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,zenburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,zenburn-fg-1))))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,zenburn-fg))))
   `(grep-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,zenburn-blue))))
   `(grep-match-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(match ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))
   `(hi-green   ((t (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))
   `(hi-pink    ((t (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))
   `(hi-yellow  ((t (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))
   `(hi-blue-b  ((t (:foreground ,zenburn-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,zenburn-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,zenburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))

   `(menu ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,zenburn-green+1
                           :background ,zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,zenburn-green-2
                      :background ,zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,zenburn-red))))
   `(vertical-border ((t (:foreground ,zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-2))))
   `(font-lock-constant-face ((t (:foreground ,zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,zenburn-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,zenburn-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-green))))
   `(anzu-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(anzu-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburn-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,zenburn-orange))))
   `(font-latex-script-char-face ((t (:foreground ,zenburn-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,zenburn-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,zenburn-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburn-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburn-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,zenburn-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,zenburn-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,zenburn-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,zenburn-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))
   `(popup-tip-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))
   `(popup-summary-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,zenburn-bg :foreground ,zenburn-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,zenburn-green+3 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,zenburn-orange :background ,zenburn-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,zenburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,zenburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,zenburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,zenburn-bg+2))))
   `(company-preview ((t (:background ,zenburn-green+2))))
   `(company-preview-common ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
   `(bm-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,zenburn-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,zenburn-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,zenburn-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,zenburn-cyan))))
   `(cfw:face-saturday ((t (:foreground ,zenburn-blue :weight bold))))
   `(cfw:face-select ((t (:background ,zenburn-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,zenburn-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,zenburn-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,zenburn-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,zenburn-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburn-green+1))))
   `(cider-deprecated-face ((t (:background ,zenburn-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,zenburn-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,zenburn-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,zenburn-red-4))))
   `(cider-test-error-face ((t (:background ,zenburn-magenta))))
   `(cider-test-success-face ((t (:background ,zenburn-green-2))))
   `(cider-fringe-good-face ((t (:foreground ,zenburn-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,zenburn-cyan))))
   `(circe-my-message-face ((t (:foreground ,zenburn-fg))))
   `(circe-fool-face ((t (:foreground ,zenburn-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,zenburn-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,zenburn-fg))))
   `(circe-server-face ((t (:foreground ,zenburn-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,zenburn-fg)))
   `(context-coloring-level-1-face ((t :foreground ,zenburn-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,zenburn-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,zenburn-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,zenburn-orange)))
   `(context-coloring-level-5-face ((t :foreground ,zenburn-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,zenburn-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,zenburn-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,zenburn-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,zenburn-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,zenburn-blue :foreground ,zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,zenburn-bg-05 :foreground ,zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,zenburn-cyan :foreground ,zenburn-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,zenburn-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,zenburn-green))))
   `(debbugs-gnu-new ((t (:foreground ,zenburn-red))))
   `(debbugs-gnu-pending ((t (:foreground ,zenburn-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,zenburn-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,zenburn-red))))
;;;;; diff
   `(diff-added          ((t (:background ,zenburn-green-5 :foreground ,zenburn-green+2))))
   `(diff-changed        ((t (:background "#555511" :foreground ,zenburn-yellow-1))))
   `(diff-removed        ((t (:background ,zenburn-red-6 :foreground ,zenburn-red+1))))
   `(diff-refine-added   ((t (:background ,zenburn-green-4 :foreground ,zenburn-green+3))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburn-yellow))))
   `(diff-refine-removed ((t (:background ,zenburn-red-5 :foreground ,zenburn-red+2))))
   `(diff-header ((,class (:background ,zenburn-bg+2))
                  (t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))
      (t (:background ,zenburn-fg :foreground ,zenburn-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,zenburn-blue :background ,zenburn-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,zenburn-red+1 :background ,zenburn-red-1))))
   `(diff-hl-insert ((,class (:foreground ,zenburn-green+1 :background ,zenburn-green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,zenburn-fg))))
   `(diredp-number ((t (:foreground ,zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,zenburn-green-2))))
   `(diredp-symlink ((t (:foreground ,zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,zenburn-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,zenburn-red :weight bold))))
   `(dired-async-message ((t (:foreground ,zenburn-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,zenburn-yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,zenburn-orange))))
   `(diredfl-date-time ((t (:foreground ,zenburn-magenta))))
   `(diredfl-deletion ((t (:foreground ,zenburn-yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,zenburn-red))))
   `(diredfl-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,zenburn-cyan))))
   `(diredfl-exec-priv ((t (:foreground ,zenburn-red))))
   `(diredfl-executable-tag ((t (:foreground ,zenburn-green+1))))
   `(diredfl-file-name ((t (:foreground ,zenburn-blue))))
   `(diredfl-file-suffix ((t (:foreground ,zenburn-green))))
   `(diredfl-flag-mark ((t (:foreground ,zenburn-yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,zenburn-orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,zenburn-red))))
   `(diredfl-link-priv ((t (:foreground ,zenburn-yellow))))
   `(diredfl-no-priv ((t (:foreground ,zenburn-fg))))
   `(diredfl-number ((t (:foreground ,zenburn-green+1))))
   `(diredfl-other-priv ((t (:foreground ,zenburn-yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,zenburn-red-1))))
   `(diredfl-read-priv ((t (:foreground ,zenburn-green-1))))
   `(diredfl-symlink ((t (:foreground ,zenburn-yellow))))
   `(diredfl-write-priv ((t (:foreground ,zenburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:inherit ediff-current-diff-A))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,zenburn-blue+2 :background ,zenburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,zenburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,zenburn-bg+1))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:inherit ediff-fine-diff-A))))
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,zenburn-blue+3 :background ,zenburn-blue-4 :weight bold))))
   `(ediff-odd-diff-A ((t (:background ,zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,zenburn-green+3))))
   `(egg-branch ((t (:foreground ,zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,zenburn-yellow))))
   `(egg-term ((t (:foreground ,zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,zenburn-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,zenburn-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,zenburn-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,zenburn-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,zenburn-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,zenburn-green))))
   `(elfeed-search-feed-face ((t (:foreground ,zenburn-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,zenburn-bg-1
                                     :foreground ,zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,zenburn-green))))
   `(erc-pal-face ((t (:foreground ,zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,zenburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,zenburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,zenburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))
      (t (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))
      (t (:foreground ,zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,zenburn-green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))
      (t (:foreground ,zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))
      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,zenburn-fg))))
   `(ack-file ((t (:foreground ,zenburn-blue))))
   `(ack-line ((t (:foreground ,zenburn-yellow))))
   `(ack-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,zenburn-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, zenburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,zenburn-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,zenburn-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,zenburn-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,zenburn-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,zenburn-yellow))))
   `(gnus-x ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))
   `(mm-uu-extract ((t (:background ,zenburn-bg-05 :foreground ,zenburn-green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,zenburn-bg-1 :background ,zenburn-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,zenburn-green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,zenburn-green
                      :background ,zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,zenburn-yellow
                      :background ,zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,zenburn-orange :background ,zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(helm-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,zenburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,zenburn-red-1 :background ,zenburn-bg))))
   `(hydra-face-amaranth ((t (:foreground ,zenburn-red-3 :background ,zenburn-bg))))
   `(hydra-face-blue ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(hydra-face-pink ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))
   `(hydra-face-teal ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(info-constant-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-function-ref-item ((t (:background ,zenburn-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-menu ((t (:foreground ,zenburn-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,zenburn-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue+1))))
   `(info-user-option-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-red))))
   `(info-variable-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,zenburn-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,zenburn-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,zenburn-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,zenburn-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,zenburn-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,zenburn-green+3))))
   `(irfc-title-face ((t (:foreground ,zenburn-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,zenburn-green :background ,zenburn-bg))))
   `(ivy-current-match ((t (:foreground ,zenburn-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(ivy-match-required-face ((t (:foreground ,zenburn-red :background ,zenburn-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-green+1))))
   `(ivy-remote ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))
   `(ivy-subdir ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,zenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,zenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,zenburn-yellow))))
   `(ido-indicator ((t (:foreground ,zenburn-yellow :background ,zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,zenburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,zenburn-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,zenburn-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,zenburn-orange))))
   `(jabber-roster-user-error ((t (:foreground ,zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburn-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,zenburn-green+3))))
   `(jabber-activity-face((t (:foreground ,zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,zenburn-orange))))
   `(js2-error ((t (:foreground ,zenburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-type ((t (:foreground ,zenburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,zenburn-green+3))))
   `(js2-function-param ((t (:foreground, zenburn-orange))))
   `(js2-external-variable ((t (:foreground ,zenburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,zenburn-green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburn-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburn-red-1))))
   `(js2-object-property ((t (:foreground ,zenburn-blue+1))))
   `(js2-magic-paren ((t (:foreground ,zenburn-blue-5))))
   `(js2-private-function-call ((t (:foreground ,zenburn-cyan))))
   `(js2-function-call ((t (:foreground ,zenburn-cyan))))
   `(js2-private-member ((t (:foreground ,zenburn-blue-1))))
   `(js2-keywords ((t (:foreground ,zenburn-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,zenburn-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,zenburn-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,zenburn-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,zenburn-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,zenburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,zenburn-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,zenburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,zenburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,zenburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,zenburn-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburn-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburn-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburn-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,zenburn-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,zenburn-blue-1))))
   `(lui-hilight-face ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,zenburn-magenta :background ,zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,zenburn-orange :weight bold))))

   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,zenburn-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,zenburn-bg+05
                                                        :foreground ,zenburn-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,zenburn-bg+2
                                                        :foreground ,zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,zenburn-orange
                                                        :foreground ,zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,zenburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,zenburn-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,zenburn-green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-date    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))
   `(magit-blame-summary ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,zenburn-yellow))))
   `(magit-signature-expired   ((t (:foreground ,zenburn-orange))))
   `(magit-signature-revoked   ((t (:foreground ,zenburn-magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,zenburn-cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,zenburn-blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,zenburn-yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,zenburn-yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,zenburn-fg+1))))
   `(markup-meta-face ((t (:foreground ,zenburn-yellow))))
   `(markup-meta-hide-face ((t (:foreground ,zenburn-yellow))))
   `(markup-secondary-text-face ((t (:foreground ,zenburn-yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,zenburn-yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,zenburn-green+1))))
   `(message-header-other ((t (:foreground ,zenburn-green))))
   `(message-header-to ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,zenburn-green))))
   `(message-mml ((t (:foreground ,zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,zenburn-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,zenburn-magenta))))
   `(mingus-song-file-face ((t (:foreground ,zenburn-yellow))))
   `(mingus-artist-face ((t (:foreground ,zenburn-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,zenburn-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,zenburn-red+1))))
   `(mingus-stopped-face ((t (:foreground ,zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,zenburn-red))))
   `(nav-face-file ((t (:foreground ,zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,zenburn-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))
      (t
       (:underline ,zenburn-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))
      (t
       (:underline ,zenburn-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,zenburn-green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,zenburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,zenburn-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,zenburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,zenburn-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,zenburn-blue))))
   `(neo-file-link-face ((t (:foreground ,zenburn-fg))))
   `(neo-expand-btn-face ((t (:foreground ,zenburn-blue))))
   `(neo-vc-default-face ((t (:foreground ,zenburn-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,zenburn-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,zenburn-fg))))
   `(neo-vc-edited-face ((t (:foreground ,zenburn-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburn-red :background ,zenburn-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,zenburn-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,zenburn-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,zenburn-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,zenburn-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,zenburn-green+3))))
   `(org-formula ((t (:foreground ,zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,zenburn-green+3))))
   `(org-hide ((t (:foreground ,zenburn-bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                               ,@(when zenburn-scale-org-headlines
                                   (list :height zenburn-height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
   `(org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,zenburn-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,zenburn-orange))))
   `(org-todo ((t (:weight bold :foreground ,zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,zenburn-bg-1))))
   `(org-column-title ((t (:background ,zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,zenburn-cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue
                                         :weight bold :height ,zenburn-height-plus-4))))
   `(org-document-info ((t (:foreground ,zenburn-blue))))
   `(org-habit-ready-face ((t :background ,zenburn-green)))
   `(org-habit-alert-face ((t :background ,zenburn-yellow-1 :foreground ,zenburn-bg)))
   `(org-habit-clear-face ((t :background ,zenburn-blue-3)))
   `(org-habit-overdue-face ((t :background ,zenburn-red-3)))
   `(org-habit-clear-future-face ((t :background ,zenburn-blue-4)))
   `(org-habit-ready-future-face ((t :background ,zenburn-green-2)))
   `(org-habit-alert-future-face ((t :background ,zenburn-yellow-2 :foreground ,zenburn-bg)))
   `(org-habit-overdue-future-face ((t :background ,zenburn-red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2
                             ,@(when zenburn-scale-outline-headlines
                                 (list :height zenburn-height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,zenburn-magenta))))
   `(cperl-array-face ((t (:foreground ,zenburn-yellow, :backgorund ,zenburn-bg))))
   `(cperl-hash-face ((t (:foreground ,zenburn-yellow-1, :background ,zenburn-bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,zenburn-fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,zenburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-error-face ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-locked-face ((t (:background ,zenburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(proof-queue-face ((t (:background ,zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,zenburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))
   `(proof-warning-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,zenburn-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,zenburn-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))
   `(reb-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))
   `(reb-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))
   `(reb-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,zenburn-green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,zenburn-yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,zenburn-orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenburn-red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,zenburn-yellow))))
   `(realgud-backtrace-number ((t (:foreground ,zenburn-yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,zenburn-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,zenburn-green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,zenburn-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Zenburn for sml
   `(sml/global ((,class (:foreground ,zenburn-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,zenburn-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,zenburn-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,zenburn-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,zenburn-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,zenburn-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,zenburn-orange))))
   `(sml/git ((,class (:foreground ,zenburn-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,zenburn-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,zenburn-red-2))))
   `(sml/outside-modified ((,class (:foreground ,zenburn-orange))))
   `(sml/modified ((,class (:foreground ,zenburn-red))))
   `(sml/vc-edited ((,class (:foreground ,zenburn-green+2))))
   `(sml/charging ((,class (:foreground ,zenburn-green+4))))
   `(sml/discharging ((,class (:foreground ,zenburn-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,zenburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-red)))
      (t
       (:underline ,zenburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-orange)))
      (t
       (:underline ,zenburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-yellow)))
      (t
       (:underline ,zenburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,zenburn-green)))
      (t
       (:underline ,zenburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,zenburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,zenburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,zenburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,zenburn-red))))
   `(speedbar-separator-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,zenburn-yellow))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,zenburn-fg :foreground ,zenburn-bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,zenburn-green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,zenburn-green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,zenburn-bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,zenburn-fg
                                    :background ,zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,zenburn-fg
                                      :background ,zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,zenburn-fg
                                        :background ,zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,zenburn-bg
                                       :background ,zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,zenburn-red-2
                                     :background ,zenburn-red-4))))
   `(term-color-green ((t (:foreground ,zenburn-green
                                       :background ,zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,zenburn-orange
                                        :background ,zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,zenburn-blue-1
                                      :background ,zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,zenburn-magenta
                                         :background ,zenburn-red))))
   `(term-color-cyan ((t (:foreground ,zenburn-cyan
                                      :background ,zenburn-blue))))
   `(term-color-white ((t (:foreground ,zenburn-fg
                                       :background ,zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburn-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,zenburn-bg :background ,zenburn-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,zenburn-yellow-2 :background ,zenburn-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,zenburn-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,zenburn-red))))
   `(whitespace-line ((t (:background ,zenburn-bg :foreground ,zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,zenburn-orange :foreground ,zenburn-orange))))
   `(whitespace-indentation ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
   `(whitespace-empty ((t (:background ,zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,zenburn-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,zenburn-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,zenburn-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,zenburn-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue+1))))
   `(cscope-separator-face ((t (:foreground ,zenburn-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))
   ))

;;; Theme Variables
(zenburn-with-color-variables
  (custom-theme-set-variables
   'zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow
                                          ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,zenburn-bg+1)
   `(company-quickhelp-color-foreground ,zenburn-fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,zenburn-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,zenburn-red ,zenburn-orange ,zenburn-yellow ,zenburn-green ,zenburn-green+4
       ,zenburn-cyan ,zenburn-blue+1 ,zenburn-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,zenburn-fg . ,zenburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,zenburn-red-1)
       ( 40. . ,zenburn-red)
       ( 60. . ,zenburn-orange)
       ( 80. . ,zenburn-yellow-2)
       (100. . ,zenburn-yellow-1)
       (120. . ,zenburn-yellow)
       (140. . ,zenburn-green-2)
       (160. . ,zenburn-green)
       (180. . ,zenburn-green+1)
       (200. . ,zenburn-green+2)
       (220. . ,zenburn-green+3)
       (240. . ,zenburn-green+4)
       (260. . ,zenburn-cyan)
       (280. . ,zenburn-blue-2)
       (300. . ,zenburn-blue-1)
       (320. . ,zenburn-blue)
       (340. . ,zenburn-blue+1)
       (360. . ,zenburn-magenta)))
   `(vc-annotate-very-old-color ,zenburn-magenta)
   `(vc-annotate-background ,zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for zenburn color names.
In buffers visiting library `zenburn-theme.el' the zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar zenburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after zenburn activate)
;;   "Maybe also add font-lock keywords for zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "zenburn-theme.el")))
;;     (unless zenburn-colors-font-lock-keywords
;;       (setq zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after zenburn activate)
;;   "Also remove font-lock keywords for zenburn colors."
;;   (font-lock-remove-keywords nil zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; zenburn-theme.el ends here</pre>O l�;;; zenburn-theme.el --- A low contrast color theme for Emacs.;; Copyright (C) 2011-2018 Bozhidar Batsov;; Author: Bozhidar Batsov <bozhidar@batsov.com>;; URL: http://github.com/bbatsov/zenburn-emacs;; Version: 2.7-snapshot;; This program is free software; you can redistribute it and/or modify;; it under the terms of the GNU General Public License as published by;; the Free Software Foundation, either version 3 of the License, or;; (at your option) any later version.;; This program is distributed in the hope that it will be useful,;; but WITHOUT ANY WARRANTY; without even the implied warranty of;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the;; GNU General Public License for more details.;; You should have received a copy of the GNU General Public License;; along with this program.  If not, see <http://www.gnu.org/licenses/>.;;; Commentary:;; A port of the popular Vim theme Zenburn for Emacs 24+, built on top;; of the new built-in theme support in Emacs 24.;;; Credits:;; Jani Nurminen created the original theme for vim on which this port;; is based.;;; Code:(deftheme zenburn "The Zenburn color theme")(defgroup zenburn-theme nil  "Zenburn theme."  :prefix "zenburn-theme-"  :link '(url-link :tag "GitHub" "http://github.com/bbatsov/zenburn-emacs")  :tag "Zenburn theme");;;###autoload(defcustom zenburn-override-colors-alist '()  "Place to override default theme colors.You can override a subset of the theme's default colors bydefining them in this alist."  :group 'zenburn-theme  :type '(alist          :key-type (string :tag "Name")          :value-type (string :tag " Hex")))(defcustom zenburn-use-variable-pitch nil  "Use variable pitch face for some headings and titles."  :type 'boolean  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-height-minus-1 0.8  "Font size -1."  :type 'number  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-height-plus-1 1.1  "Font size +1."  :type 'number  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-height-plus-2 1.15  "Font size +2."  :type 'number  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-height-plus-3 1.2  "Font size +3."  :type 'number  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-height-plus-4 1.3  "Font size +4."  :type 'number  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-scale-org-headlines nil  "Whether `org-mode' headlines should be scaled."  :type 'boolean  :group 'zenburn-theme  :package-version '(zenburn . "2.6"))(defcustom zenburn-scale-outline-headlines nil  "Whether `outline-mode' headlines should be scaled."  :type 'boolean  :group 'zenburn-theme  :package-version '(zenburn . "2.6"));;; Color Palette(defvar zenburn-default-colors-alist  '(("zenburn-fg+1"     . "#FFFFEF")    ("zenburn-fg"       . "#DCDCCC")    ("zenburn-fg-1"     . "#656555")    ("zenburn-bg-2"     . "#000000")    ("zenburn-bg-1"     . "#2B2B2B")    ("zenburn-bg-05"    . "#383838")    ("zenburn-bg"       . "#3F3F3F")    ("zenburn-bg+05"    . "#494949")    ("zenburn-bg+1"     . "#4F4F4F")    ("zenburn-bg+2"     . "#5F5F5F")    ("zenburn-bg+3"     . "#6F6F6F")    ("zenburn-red+2"    . "#ECB3B3")    ("zenburn-red+1"    . "#DCA3A3")    ("zenburn-red"      . "#CC9393")    ("zenburn-red-1"    . "#BC8383")    ("zenburn-red-2"    . "#AC7373")    ("zenburn-red-3"    . "#9C6363")    ("zenburn-red-4"    . "#8C5353")    ("zenburn-red-5"    . "#7C4343")    ("zenburn-red-6"    . "#6C3333")    ("zenburn-orange"   . "#DFAF8F")    ("zenburn-yellow"   . "#F0DFAF")    ("zenburn-yellow-1" . "#E0CF9F")    ("zenburn-yellow-2" . "#D0BF8F")    ("zenburn-green-5"  . "#2F4F2F")    ("zenburn-green-4"  . "#3F5F3F")    ("zenburn-green-3"  . "#4F6F4F")    ("zenburn-green-2"  . "#5F7F5F")    ("zenburn-green-1"  . "#6F8F6F")    ("zenburn-green"    . "#7F9F7F")    ("zenburn-green+1"  . "#8FB28F")    ("zenburn-green+2"  . "#9FC59F")    ("zenburn-green+3"  . "#AFD8AF")    ("zenburn-green+4"  . "#BFEBBF")    ("zenburn-cyan"     . "#93E0E3")    ("zenburn-blue+3"   . "#BDE0F3")    ("zenburn-blue+2"   . "#ACE0E3")    ("zenburn-blue+1"   . "#94BFF3")    ("zenburn-blue"     . "#8CD0D3")    ("zenburn-blue-1"   . "#7CB8BB")    ("zenburn-blue-2"   . "#6CA0A3")    ("zenburn-blue-3"   . "#5C888B")    ("zenburn-blue-4"   . "#4C7073")    ("zenburn-blue-5"   . "#366060")    ("zenburn-magenta"  . "#DC8CC3"))  "List of Zenburn colors.Each element has the form (NAME . HEX).`+N' suffixes indicate a color is lighter.`-N' suffixes indicate a color is darker.")(defmacro zenburn-with-color-variables (&rest body)  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.Also bind `class' to ((class color) (min-colors 89))."  (declare (indent 0))  `(let ((class '((class color) (min-colors 89)))         ,@(mapcar (lambda (cons)                     (list (intern (car cons)) (cdr cons)))                   (append zenburn-default-colors-alist                           zenburn-override-colors-alist))         (z-variable-pitch (if zenburn-use-variable-pitch                               'variable-pitch 'default)))     ,@body));;; Theme Faces(zenburn-with-color-variables  (custom-theme-set-faces   'zenburn;;;; Built-in;;;;; basic coloring   '(button ((t (:underline t))))   `(link ((t (:foreground ,zenburn-yellow :underline t :weight bold))))   `(link-visited ((t (:foreground ,zenburn-yellow-2 :underline t :weight normal))))   `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))   `(cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg+1))))   `(widget-field ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3))))   `(escape-glyph ((t (:foreground ,zenburn-yellow :weight bold))))   `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))   `(header-line ((t (:foreground ,zenburn-yellow                                  :background ,zenburn-bg-1                                  :box (:line-width -1 :style released-button)))))   `(highlight ((t (:background ,zenburn-bg-05))))   `(success ((t (:foreground ,zenburn-green :weight bold))))   `(warning ((t (:foreground ,zenburn-orange :weight bold))))   `(tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))));;;;; compilation   `(compilation-column-face ((t (:foreground ,zenburn-yellow))))   `(compilation-enter-directory-face ((t (:foreground ,zenburn-green))))   `(compilation-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))   `(compilation-face ((t (:foreground ,zenburn-fg))))   `(compilation-info-face ((t (:foreground ,zenburn-blue))))   `(compilation-info ((t (:foreground ,zenburn-green+4 :underline t))))   `(compilation-leave-directory-face ((t (:foreground ,zenburn-green))))   `(compilation-line-face ((t (:foreground ,zenburn-yellow))))   `(compilation-line-number ((t (:foreground ,zenburn-yellow))))   `(compilation-message-face ((t (:foreground ,zenburn-blue))))   `(compilation-warning-face ((t (:foreground ,zenburn-orange :weight bold :underline t))))   `(compilation-mode-line-exit ((t (:foreground ,zenburn-green+2 :weight bold))))   `(compilation-mode-line-fail ((t (:foreground ,zenburn-red :weight bold))))   `(compilation-mode-line-run ((t (:foreground ,zenburn-yellow :weight bold))));;;;; completions   `(completions-annotations ((t (:foreground ,zenburn-fg-1))));;;;; eww   '(eww-invalid-certificate ((t (:inherit error))))   '(eww-valid-certificate   ((t (:inherit success))));;;;; grep   `(grep-context-face ((t (:foreground ,zenburn-fg))))   `(grep-error-face ((t (:foreground ,zenburn-red-1 :weight bold :underline t))))   `(grep-hit-face ((t (:foreground ,zenburn-blue))))   `(grep-match-face ((t (:foreground ,zenburn-orange :weight bold))))   `(match ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange :weight bold))));;;;; hi-lock   `(hi-blue    ((t (:background ,zenburn-cyan    :foreground ,zenburn-bg-1))))   `(hi-green   ((t (:background ,zenburn-green+4 :foreground ,zenburn-bg-1))))   `(hi-pink    ((t (:background ,zenburn-magenta :foreground ,zenburn-bg-1))))   `(hi-yellow  ((t (:background ,zenburn-yellow  :foreground ,zenburn-bg-1))))   `(hi-blue-b  ((t (:foreground ,zenburn-blue    :weight     bold))))   `(hi-green-b ((t (:foreground ,zenburn-green+2 :weight     bold))))   `(hi-red-b   ((t (:foreground ,zenburn-red     :weight     bold))));;;;; info   `(Info-quoted ((t (:inherit font-lock-constant-face))));;;;; isearch   `(isearch ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg+2))))   `(isearch-fail ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))   `(lazy-highlight ((t (:foreground ,zenburn-yellow-2 :weight bold :background ,zenburn-bg-05))))   `(menu ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))   `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))   `(mode-line     ((,class (:foreground ,zenburn-green+1                           :background ,zenburn-bg-1                           :box (:line-width -1 :style released-button)))      (t :inverse-video t)))   `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))   `(mode-line-inactive     ((t (:foreground ,zenburn-green-2                      :background ,zenburn-bg-05                      :box (:line-width -1 :style released-button)))))   `(region ((,class (:background ,zenburn-bg-1))             (t :inverse-video t)))   `(secondary-selection ((t (:background ,zenburn-bg+2))))   `(trailing-whitespace ((t (:background ,zenburn-red))))   `(vertical-border ((t (:foreground ,zenburn-fg))));;;;; font lock   `(font-lock-builtin-face ((t (:foreground ,zenburn-fg :weight bold))))   `(font-lock-comment-face ((t (:foreground ,zenburn-green))))   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-2))))   `(font-lock-constant-face ((t (:foreground ,zenburn-green+4))))   `(font-lock-doc-face ((t (:foreground ,zenburn-green+2))))   `(font-lock-function-name-face ((t (:foreground ,zenburn-cyan))))   `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))   `(font-lock-negation-char-face ((t (:foreground ,zenburn-yellow :weight bold))))   `(font-lock-preprocessor-face ((t (:foreground ,zenburn-blue+1))))   `(font-lock-regexp-grouping-construct ((t (:foreground ,zenburn-yellow :weight bold))))   `(font-lock-regexp-grouping-backslash ((t (:foreground ,zenburn-green :weight bold))))   `(font-lock-string-face ((t (:foreground ,zenburn-red))))   `(font-lock-type-face ((t (:foreground ,zenburn-blue-1))))   `(font-lock-variable-name-face ((t (:foreground ,zenburn-orange))))   `(font-lock-warning-face ((t (:foreground ,zenburn-yellow-2 :weight bold))))   `(c-annotation-face ((t (:inherit font-lock-constant-face))));;;;; line numbers (Emacs 26.1 and above)   `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg-05))))   `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2))));;;;; man   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))));;;;; newsticker   `(newsticker-date-face ((t (:foreground ,zenburn-fg))))   `(newsticker-default-face ((t (:foreground ,zenburn-fg))))   `(newsticker-enclosure-face ((t (:foreground ,zenburn-green+3))))   `(newsticker-extra-face ((t (:foreground ,zenburn-bg+2 :height 0.8))))   `(newsticker-feed-face ((t (:foreground ,zenburn-fg))))   `(newsticker-immortal-item-face ((t (:foreground ,zenburn-green))))   `(newsticker-new-item-face ((t (:foreground ,zenburn-blue))))   `(newsticker-obsolete-item-face ((t (:foreground ,zenburn-red))))   `(newsticker-old-item-face ((t (:foreground ,zenburn-bg+3))))   `(newsticker-statistics-face ((t (:foreground ,zenburn-fg))))   `(newsticker-treeview-face ((t (:foreground ,zenburn-fg))))   `(newsticker-treeview-immortal-face ((t (:foreground ,zenburn-green))))   `(newsticker-treeview-listwindow-face ((t (:foreground ,zenburn-fg))))   `(newsticker-treeview-new-face ((t (:foreground ,zenburn-blue :weight bold))))   `(newsticker-treeview-obsolete-face ((t (:foreground ,zenburn-red))))   `(newsticker-treeview-old-face ((t (:foreground ,zenburn-bg+3))))   `(newsticker-treeview-selection-face ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))));;;;; woman   '(woman-bold   ((t (:inherit font-lock-keyword-face))))   '(woman-italic ((t (:inherit (font-lock-string-face italic)))));;;; Third-party;;;;; ace-jump   `(ace-jump-face-background     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))   `(ace-jump-face-foreground     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :inverse-video nil))));;;;; ace-window   `(aw-background-face     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))));;;;; android mode   `(android-mode-debug-face ((t (:foreground ,zenburn-green+1))))   `(android-mode-error-face ((t (:foreground ,zenburn-orange :weight bold))))   `(android-mode-info-face ((t (:foreground ,zenburn-fg))))   `(android-mode-verbose-face ((t (:foreground ,zenburn-green))))   `(android-mode-warning-face ((t (:foreground ,zenburn-yellow))));;;;; anzu   `(anzu-mode-line ((t (:foreground ,zenburn-cyan :weight bold))))   `(anzu-mode-line-no-match ((t (:foreground ,zenburn-red :weight bold))))   `(anzu-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-green))))   `(anzu-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))   `(anzu-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,zenburn-yellow))));;;;; auctex   `(font-latex-bold-face ((t (:inherit bold))))   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))   `(font-latex-sectioning-5-face ((t (:foreground ,zenburn-red :weight bold ))))   `(font-latex-sedate-face ((t (:foreground ,zenburn-yellow))))   `(font-latex-italic-face ((t (:foreground ,zenburn-cyan :slant italic))))   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))   `(font-latex-math-face ((t (:foreground ,zenburn-orange))))   `(font-latex-script-char-face ((t (:foreground ,zenburn-orange))));;;;; agda-mode   `(agda2-highlight-keyword-face ((t (:foreground ,zenburn-yellow :weight bold))))   `(agda2-highlight-string-face ((t (:foreground ,zenburn-red))))   `(agda2-highlight-symbol-face ((t (:foreground ,zenburn-orange))))   `(agda2-highlight-primitive-type-face ((t (:foreground ,zenburn-blue-1))))   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,zenburn-fg))))   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,zenburn-fg))))   `(agda2-highlight-datatype-face ((t (:foreground ,zenburn-blue))))   `(agda2-highlight-function-face ((t (:foreground ,zenburn-blue))))   `(agda2-highlight-module-face ((t (:foreground ,zenburn-blue-1))))   `(agda2-highlight-error-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(agda2-highlight-termination-problem-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(agda2-highlight-typechecks-face ((t (:background ,zenburn-red-4))));;;;; auto-complete   `(ac-candidate-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))   `(ac-selection-face ((t (:background ,zenburn-blue-4 :foreground ,zenburn-fg))))   `(popup-tip-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))   `(popup-menu-mouse-face ((t (:background ,zenburn-yellow-2 :foreground ,zenburn-bg-2))))   `(popup-summary-face ((t (:background ,zenburn-bg+3 :foreground ,zenburn-bg-2))))   `(popup-scroll-bar-foreground-face ((t (:background ,zenburn-blue-5))))   `(popup-scroll-bar-background-face ((t (:background ,zenburn-bg-1))))   `(popup-isearch-match ((t (:background ,zenburn-bg :foreground ,zenburn-fg))));;;;; avy   `(avy-background-face     ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg :inverse-video nil))))   `(avy-lead-face-0     ((t (:foreground ,zenburn-green+3 :background ,zenburn-bg :inverse-video nil :weight bold))))   `(avy-lead-face-1     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :inverse-video nil :weight bold))))   `(avy-lead-face-2     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg :inverse-video nil :weight bold))))   `(avy-lead-face     ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :inverse-video nil :weight bold))));;;;; company-mode   `(company-tooltip ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))   `(company-tooltip-annotation ((t (:foreground ,zenburn-orange :background ,zenburn-bg+1))))   `(company-tooltip-annotation-selection ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1))))   `(company-tooltip-selection ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))   `(company-tooltip-mouse ((t (:background ,zenburn-bg-1))))   `(company-tooltip-common ((t (:foreground ,zenburn-green+2))))   `(company-tooltip-common-selection ((t (:foreground ,zenburn-green+2))))   `(company-scrollbar-fg ((t (:background ,zenburn-bg-1))))   `(company-scrollbar-bg ((t (:background ,zenburn-bg+2))))   `(company-preview ((t (:background ,zenburn-green+2))))   `(company-preview-common ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))));;;;; bm   `(bm-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))   `(bm-fringe-face ((t (:background ,zenburn-yellow-1 :foreground ,zenburn-bg))))   `(bm-fringe-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))))   `(bm-persistent-face ((t (:background ,zenburn-green-2 :foreground ,zenburn-bg))));;;;; calfw   `(cfw:face-annotation ((t (:foreground ,zenburn-red :inherit cfw:face-day-title))))   `(cfw:face-day-title ((t nil)))   `(cfw:face-default-content ((t (:foreground ,zenburn-green))))   `(cfw:face-default-day ((t (:weight bold))))   `(cfw:face-disable ((t (:foreground ,zenburn-fg-1))))   `(cfw:face-grid ((t (:inherit shadow))))   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))   `(cfw:face-periods ((t (:foreground ,zenburn-cyan))))   `(cfw:face-saturday ((t (:foreground ,zenburn-blue :weight bold))))   `(cfw:face-select ((t (:background ,zenburn-blue-5))))   `(cfw:face-sunday ((t (:foreground ,zenburn-red :weight bold))))   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))   `(cfw:face-today ((t (:foreground ,zenburn-cyan :weight bold))))   `(cfw:face-today-title ((t (:inherit highlight bold))))   `(cfw:face-toolbar ((t (:background ,zenburn-blue-5))))   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))));;;;; cider   `(cider-result-overlay-face ((t (:background unspecified))))   `(cider-enlightened-face ((t (:box (:color ,zenburn-orange :line-width -1)))))   `(cider-enlightened-local-face ((t (:weight bold :foreground ,zenburn-green+1))))   `(cider-deprecated-face ((t (:background ,zenburn-yellow-2))))   `(cider-instrumented-face ((t (:box (:color ,zenburn-red :line-width -1)))))   `(cider-traced-face ((t (:box (:color ,zenburn-cyan :line-width -1)))))   `(cider-test-failure-face ((t (:background ,zenburn-red-4))))   `(cider-test-error-face ((t (:background ,zenburn-magenta))))   `(cider-test-success-face ((t (:background ,zenburn-green-2))))   `(cider-fringe-good-face ((t (:foreground ,zenburn-green+4))));;;;; circe   `(circe-highlight-nick-face ((t (:foreground ,zenburn-cyan))))   `(circe-my-message-face ((t (:foreground ,zenburn-fg))))   `(circe-fool-face ((t (:foreground ,zenburn-red+1))))   `(circe-topic-diff-removed-face ((t (:foreground ,zenburn-red :weight bold))))   `(circe-originator-face ((t (:foreground ,zenburn-fg))))   `(circe-server-face ((t (:foreground ,zenburn-green))))   `(circe-topic-diff-new-face ((t (:foreground ,zenburn-orange :weight bold))))   `(circe-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))));;;;; context-coloring   `(context-coloring-level-0-face ((t :foreground ,zenburn-fg)))   `(context-coloring-level-1-face ((t :foreground ,zenburn-cyan)))   `(context-coloring-level-2-face ((t :foreground ,zenburn-green+4)))   `(context-coloring-level-3-face ((t :foreground ,zenburn-yellow)))   `(context-coloring-level-4-face ((t :foreground ,zenburn-orange)))   `(context-coloring-level-5-face ((t :foreground ,zenburn-magenta)))   `(context-coloring-level-6-face ((t :foreground ,zenburn-blue+1)))   `(context-coloring-level-7-face ((t :foreground ,zenburn-green+2)))   `(context-coloring-level-8-face ((t :foreground ,zenburn-yellow-2)))   `(context-coloring-level-9-face ((t :foreground ,zenburn-red+1)));;;;; coq   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))));;;;; ctable   `(ctbl:face-cell-select ((t (:background ,zenburn-blue :foreground ,zenburn-bg))))   `(ctbl:face-continue-bar ((t (:background ,zenburn-bg-05 :foreground ,zenburn-bg))))   `(ctbl:face-row-select ((t (:background ,zenburn-cyan :foreground ,zenburn-bg))));;;;; debbugs   `(debbugs-gnu-done ((t (:foreground ,zenburn-fg-1))))   `(debbugs-gnu-handled ((t (:foreground ,zenburn-green))))   `(debbugs-gnu-new ((t (:foreground ,zenburn-red))))   `(debbugs-gnu-pending ((t (:foreground ,zenburn-blue))))   `(debbugs-gnu-stale ((t (:foreground ,zenburn-orange))))   `(debbugs-gnu-tagged ((t (:foreground ,zenburn-red))));;;;; diff   `(diff-added          ((t (:background ,zenburn-green-5 :foreground ,zenburn-green+2))))   `(diff-changed        ((t (:background "#555511" :foreground ,zenburn-yellow-1))))   `(diff-removed        ((t (:background ,zenburn-red-6 :foreground ,zenburn-red+1))))   `(diff-refine-added   ((t (:background ,zenburn-green-4 :foreground ,zenburn-green+3))))   `(diff-refine-changed ((t (:background "#888811" :foreground ,zenburn-yellow))))   `(diff-refine-removed ((t (:background ,zenburn-red-5 :foreground ,zenburn-red+2))))   `(diff-header ((,class (:background ,zenburn-bg+2))                  (t (:background ,zenburn-fg :foreground ,zenburn-bg))))   `(diff-file-header     ((,class (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))      (t (:background ,zenburn-fg :foreground ,zenburn-bg :weight bold))));;;;; diff-hl   `(diff-hl-change ((,class (:foreground ,zenburn-blue :background ,zenburn-blue-2))))   `(diff-hl-delete ((,class (:foreground ,zenburn-red+1 :background ,zenburn-red-1))))   `(diff-hl-insert ((,class (:foreground ,zenburn-green+1 :background ,zenburn-green-2))));;;;; dim-autoload   `(dim-autoload-cookie-line ((t :foreground ,zenburn-bg+1)));;;;; dired+   `(diredp-display-msg ((t (:foreground ,zenburn-blue))))   `(diredp-compressed-file-suffix ((t (:foreground ,zenburn-orange))))   `(diredp-date-time ((t (:foreground ,zenburn-magenta))))   `(diredp-deletion ((t (:foreground ,zenburn-yellow))))   `(diredp-deletion-file-name ((t (:foreground ,zenburn-red))))   `(diredp-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))   `(diredp-dir-priv ((t (:foreground ,zenburn-cyan))))   `(diredp-exec-priv ((t (:foreground ,zenburn-red))))   `(diredp-executable-tag ((t (:foreground ,zenburn-green+1))))   `(diredp-file-name ((t (:foreground ,zenburn-blue))))   `(diredp-file-suffix ((t (:foreground ,zenburn-green))))   `(diredp-flag-mark ((t (:foreground ,zenburn-yellow))))   `(diredp-flag-mark-line ((t (:foreground ,zenburn-orange))))   `(diredp-ignored-file-name ((t (:foreground ,zenburn-red))))   `(diredp-link-priv ((t (:foreground ,zenburn-yellow))))   `(diredp-mode-line-flagged ((t (:foreground ,zenburn-yellow))))   `(diredp-mode-line-marked ((t (:foreground ,zenburn-orange))))   `(diredp-no-priv ((t (:foreground ,zenburn-fg))))   `(diredp-number ((t (:foreground ,zenburn-green+1))))   `(diredp-other-priv ((t (:foreground ,zenburn-yellow-1))))   `(diredp-rare-priv ((t (:foreground ,zenburn-red-1))))   `(diredp-read-priv ((t (:foreground ,zenburn-green-2))))   `(diredp-symlink ((t (:foreground ,zenburn-yellow))))   `(diredp-write-priv ((t (:foreground ,zenburn-magenta))));;;;; dired-async   `(dired-async-failures ((t (:foreground ,zenburn-red :weight bold))))   `(dired-async-message ((t (:foreground ,zenburn-yellow :weight bold))))   `(dired-async-mode-message ((t (:foreground ,zenburn-yellow))));;;;; diredfl   `(diredfl-compressed-file-suffix ((t (:foreground ,zenburn-orange))))   `(diredfl-date-time ((t (:foreground ,zenburn-magenta))))   `(diredfl-deletion ((t (:foreground ,zenburn-yellow))))   `(diredfl-deletion-file-name ((t (:foreground ,zenburn-red))))   `(diredfl-dir-heading ((t (:foreground ,zenburn-blue :background ,zenburn-bg-1))))   `(diredfl-dir-priv ((t (:foreground ,zenburn-cyan))))   `(diredfl-exec-priv ((t (:foreground ,zenburn-red))))   `(diredfl-executable-tag ((t (:foreground ,zenburn-green+1))))   `(diredfl-file-name ((t (:foreground ,zenburn-blue))))   `(diredfl-file-suffix ((t (:foreground ,zenburn-green))))   `(diredfl-flag-mark ((t (:foreground ,zenburn-yellow))))   `(diredfl-flag-mark-line ((t (:foreground ,zenburn-orange))))   `(diredfl-ignored-file-name ((t (:foreground ,zenburn-red))))   `(diredfl-link-priv ((t (:foreground ,zenburn-yellow))))   `(diredfl-no-priv ((t (:foreground ,zenburn-fg))))   `(diredfl-number ((t (:foreground ,zenburn-green+1))))   `(diredfl-other-priv ((t (:foreground ,zenburn-yellow-1))))   `(diredfl-rare-priv ((t (:foreground ,zenburn-red-1))))   `(diredfl-read-priv ((t (:foreground ,zenburn-green-1))))   `(diredfl-symlink ((t (:foreground ,zenburn-yellow))))   `(diredfl-write-priv ((t (:foreground ,zenburn-magenta))));;;;; ediff   `(ediff-current-diff-A ((t (:inherit diff-removed))))   `(ediff-current-diff-Ancestor ((t (:inherit ediff-current-diff-A))))   `(ediff-current-diff-B ((t (:inherit diff-added))))   `(ediff-current-diff-C ((t (:foreground ,zenburn-blue+2 :background ,zenburn-blue-5))))   `(ediff-even-diff-A ((t (:background ,zenburn-bg+1))))   `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg+1))))   `(ediff-even-diff-B ((t (:background ,zenburn-bg+1))))   `(ediff-even-diff-C ((t (:background ,zenburn-bg+1))))   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed :weight bold))))   `(ediff-fine-diff-Ancestor ((t (:inherit ediff-fine-diff-A))))   `(ediff-fine-diff-B ((t (:inherit diff-refine-added :weight bold))))   `(ediff-fine-diff-C ((t (:foreground ,zenburn-blue+3 :background ,zenburn-blue-4 :weight bold))))   `(ediff-odd-diff-A ((t (:background ,zenburn-bg+2))))   `(ediff-odd-diff-Ancestor ((t (:inherit ediff-odd-diff-A))))   `(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))   `(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))));;;;; egg   `(egg-text-base ((t (:foreground ,zenburn-fg))))   `(egg-help-header-1 ((t (:foreground ,zenburn-yellow))))   `(egg-help-header-2 ((t (:foreground ,zenburn-green+3))))   `(egg-branch ((t (:foreground ,zenburn-yellow))))   `(egg-branch-mono ((t (:foreground ,zenburn-yellow))))   `(egg-term ((t (:foreground ,zenburn-yellow))))   `(egg-diff-add ((t (:foreground ,zenburn-green+4))))   `(egg-diff-del ((t (:foreground ,zenburn-red+1))))   `(egg-diff-file-header ((t (:foreground ,zenburn-yellow-2))))   `(egg-section-title ((t (:foreground ,zenburn-yellow))))   `(egg-stash-mono ((t (:foreground ,zenburn-green+4))));;;;; elfeed   `(elfeed-log-error-level-face ((t (:foreground ,zenburn-red))))   `(elfeed-log-info-level-face ((t (:foreground ,zenburn-blue))))   `(elfeed-log-warn-level-face ((t (:foreground ,zenburn-yellow))))   `(elfeed-search-date-face ((t (:foreground ,zenburn-yellow-1 :underline t                                              :weight bold))))   `(elfeed-search-tag-face ((t (:foreground ,zenburn-green))))   `(elfeed-search-feed-face ((t (:foreground ,zenburn-cyan))));;;;; emacs-w3m   `(w3m-anchor ((t (:foreground ,zenburn-yellow :underline t                                 :weight bold))))   `(w3m-arrived-anchor ((t (:foreground ,zenburn-yellow-2                                         :underline t :weight normal))))   `(w3m-form ((t (:foreground ,zenburn-red-1 :underline t))))   `(w3m-header-line-location-title ((t (:foreground ,zenburn-yellow                                                     :underline t :weight bold))))   '(w3m-history-current-url ((t (:inherit match))))   `(w3m-lnum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))   `(w3m-lnum-match ((t (:background ,zenburn-bg-1                                     :foreground ,zenburn-orange                                     :weight bold))))   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,zenburn-yellow))));;;;; erc   `(erc-action-face ((t (:inherit erc-default-face))))   `(erc-bold-face ((t (:weight bold))))   `(erc-current-nick-face ((t (:foreground ,zenburn-blue :weight bold))))   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))   `(erc-default-face ((t (:foreground ,zenburn-fg))))   `(erc-direct-msg-face ((t (:inherit erc-default-face))))   `(erc-error-face ((t (:inherit font-lock-warning-face))))   `(erc-fool-face ((t (:inherit erc-default-face))))   `(erc-highlight-face ((t (:inherit hover-highlight))))   `(erc-input-face ((t (:foreground ,zenburn-yellow))))   `(erc-keyword-face ((t (:foreground ,zenburn-blue :weight bold))))   `(erc-nick-default-face ((t (:foreground ,zenburn-yellow :weight bold))))   `(erc-my-nick-face ((t (:foreground ,zenburn-red :weight bold))))   `(erc-nick-msg-face ((t (:inherit erc-default-face))))   `(erc-notice-face ((t (:foreground ,zenburn-green))))   `(erc-pal-face ((t (:foreground ,zenburn-orange :weight bold))))   `(erc-prompt-face ((t (:foreground ,zenburn-orange :background ,zenburn-bg :weight bold))))   `(erc-timestamp-face ((t (:foreground ,zenburn-green+4))))   `(erc-underline-face ((t (:underline t))));;;;; eros   `(eros-result-overlay-face ((t (:background unspecified))));;;;; ert   `(ert-test-result-expected ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg))))   `(ert-test-result-unexpected ((t (:foreground ,zenburn-red :background ,zenburn-bg))));;;;; eshell   `(eshell-prompt ((t (:foreground ,zenburn-yellow :weight bold))))   `(eshell-ls-archive ((t (:foreground ,zenburn-red-1 :weight bold))))   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))   `(eshell-ls-directory ((t (:foreground ,zenburn-blue+1 :weight bold))))   `(eshell-ls-executable ((t (:foreground ,zenburn-red+1 :weight bold))))   `(eshell-ls-unreadable ((t (:foreground ,zenburn-fg))))   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))   `(eshell-ls-special ((t (:foreground ,zenburn-yellow :weight bold))))   `(eshell-ls-symlink ((t (:foreground ,zenburn-cyan :weight bold))));;;;; flx   `(flx-highlight-face ((t (:foreground ,zenburn-green+2 :weight bold))));;;;; flycheck   `(flycheck-error     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-red-1) :inherit unspecified))      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))   `(flycheck-warning     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-yellow) :inherit unspecified))      (t (:foreground ,zenburn-yellow :weight bold :underline t))))   `(flycheck-info     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-cyan) :inherit unspecified))      (t (:foreground ,zenburn-cyan :weight bold :underline t))))   `(flycheck-fringe-error ((t (:foreground ,zenburn-red-1 :weight bold))))   `(flycheck-fringe-warning ((t (:foreground ,zenburn-yellow :weight bold))))   `(flycheck-fringe-info ((t (:foreground ,zenburn-cyan :weight bold))));;;;; flymake   `(flymake-errline     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-red)                   :inherit unspecified :foreground unspecified :background unspecified))      (t (:foreground ,zenburn-red-1 :weight bold :underline t))))   `(flymake-warnline     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-orange)                   :inherit unspecified :foreground unspecified :background unspecified))      (t (:foreground ,zenburn-orange :weight bold :underline t))))   `(flymake-infoline     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-green)                   :inherit unspecified :foreground unspecified :background unspecified))      (t (:foreground ,zenburn-green-2 :weight bold :underline t))));;;;; flyspell   `(flyspell-duplicate     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-orange) :inherit unspecified))      (t (:foreground ,zenburn-orange :weight bold :underline t))))   `(flyspell-incorrect     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-red) :inherit unspecified))      (t (:foreground ,zenburn-red-1 :weight bold :underline t))));;;;; full-ack   `(ack-separator ((t (:foreground ,zenburn-fg))))   `(ack-file ((t (:foreground ,zenburn-blue))))   `(ack-line ((t (:foreground ,zenburn-yellow))))   `(ack-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))));;;;; git-annex   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))));;;;; git-commit   `(git-commit-comment-action  ((,class (:foreground ,zenburn-green+1 :weight bold))))   `(git-commit-comment-branch  ((,class (:foreground ,zenburn-blue+1  :weight bold)))) ; obsolete   `(git-commit-comment-branch-local  ((,class (:foreground ,zenburn-blue+1  :weight bold))))   `(git-commit-comment-branch-remote ((,class (:foreground ,zenburn-green  :weight bold))))   `(git-commit-comment-heading ((,class (:foreground ,zenburn-yellow  :weight bold))));;;;; git-gutter   `(git-gutter:added ((t (:foreground ,zenburn-green :weight bold :inverse-video t))))   `(git-gutter:deleted ((t (:foreground ,zenburn-red :weight bold :inverse-video t))))   `(git-gutter:modified ((t (:foreground ,zenburn-magenta :weight bold :inverse-video t))))   `(git-gutter:unchanged ((t (:foreground ,zenburn-fg :weight bold :inverse-video t))));;;;; git-gutter-fr   `(git-gutter-fr:added ((t (:foreground ,zenburn-green  :weight bold))))   `(git-gutter-fr:deleted ((t (:foreground ,zenburn-red :weight bold))))   `(git-gutter-fr:modified ((t (:foreground ,zenburn-magenta :weight bold))));;;;; git-rebase   `(git-rebase-hash ((t (:foreground, zenburn-orange))));;;;; gnus   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))   `(gnus-header-content ((t (:inherit message-header-other))))   `(gnus-header-from ((t (:inherit message-header-to))))   `(gnus-header-name ((t (:inherit message-header-name))))   `(gnus-header-newsgroups ((t (:inherit message-header-other))))   `(gnus-header-subject ((t (:inherit message-header-subject))))   `(gnus-server-opened ((t (:foreground ,zenburn-green+2 :weight bold))))   `(gnus-server-denied ((t (:foreground ,zenburn-red+1 :weight bold))))   `(gnus-server-closed ((t (:foreground ,zenburn-blue :slant italic))))   `(gnus-server-offline ((t (:foreground ,zenburn-yellow :weight bold))))   `(gnus-server-agent ((t (:foreground ,zenburn-blue :weight bold))))   `(gnus-summary-cancelled ((t (:foreground ,zenburn-orange))))   `(gnus-summary-high-ancient ((t (:foreground ,zenburn-blue))))   `(gnus-summary-high-read ((t (:foreground ,zenburn-green :weight bold))))   `(gnus-summary-high-ticked ((t (:foreground ,zenburn-orange :weight bold))))   `(gnus-summary-high-unread ((t (:foreground ,zenburn-fg :weight bold))))   `(gnus-summary-low-ancient ((t (:foreground ,zenburn-blue))))   `(gnus-summary-low-read ((t (:foreground ,zenburn-green))))   `(gnus-summary-low-ticked ((t (:foreground ,zenburn-orange :weight bold))))   `(gnus-summary-low-unread ((t (:foreground ,zenburn-fg))))   `(gnus-summary-normal-ancient ((t (:foreground ,zenburn-blue))))   `(gnus-summary-normal-read ((t (:foreground ,zenburn-green))))   `(gnus-summary-normal-ticked ((t (:foreground ,zenburn-orange :weight bold))))   `(gnus-summary-normal-unread ((t (:foreground ,zenburn-fg))))   `(gnus-summary-selected ((t (:foreground ,zenburn-yellow :weight bold))))   `(gnus-cite-1 ((t (:foreground ,zenburn-blue))))   `(gnus-cite-10 ((t (:foreground ,zenburn-yellow-1))))   `(gnus-cite-11 ((t (:foreground ,zenburn-yellow))))   `(gnus-cite-2 ((t (:foreground ,zenburn-blue-1))))   `(gnus-cite-3 ((t (:foreground ,zenburn-blue-2))))   `(gnus-cite-4 ((t (:foreground ,zenburn-green+2))))   `(gnus-cite-5 ((t (:foreground ,zenburn-green+1))))   `(gnus-cite-6 ((t (:foreground ,zenburn-green))))   `(gnus-cite-7 ((t (:foreground ,zenburn-red))))   `(gnus-cite-8 ((t (:foreground ,zenburn-red-1))))   `(gnus-cite-9 ((t (:foreground ,zenburn-red-2))))   `(gnus-group-news-1-empty ((t (:foreground ,zenburn-yellow))))   `(gnus-group-news-2-empty ((t (:foreground ,zenburn-green+3))))   `(gnus-group-news-3-empty ((t (:foreground ,zenburn-green+1))))   `(gnus-group-news-4-empty ((t (:foreground ,zenburn-blue-2))))   `(gnus-group-news-5-empty ((t (:foreground ,zenburn-blue-3))))   `(gnus-group-news-6-empty ((t (:foreground ,zenburn-bg+2))))   `(gnus-group-news-low-empty ((t (:foreground ,zenburn-bg+2))))   `(gnus-signature ((t (:foreground ,zenburn-yellow))))   `(gnus-x ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))   `(mm-uu-extract ((t (:background ,zenburn-bg-05 :foreground ,zenburn-green+1))));;;;; go-guru   `(go-guru-hl-identifier-face ((t (:foreground ,zenburn-bg-1 :background ,zenburn-green+1))));;;;; guide-key   `(guide-key/highlight-command-face ((t (:foreground ,zenburn-blue))))   `(guide-key/key-face ((t (:foreground ,zenburn-green))))   `(guide-key/prefix-command-face ((t (:foreground ,zenburn-green+1))));;;;; hackernews   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))   '(hackernews-link          ((t (:inherit link         :underline nil))));;;;; helm   `(helm-header     ((t (:foreground ,zenburn-green                      :background ,zenburn-bg                      :underline nil                      :box nil))))   `(helm-source-header     ((t (:foreground ,zenburn-yellow                      :background ,zenburn-bg-1                      :underline nil                      :weight bold                      :box (:line-width -1 :style released-button)))))   `(helm-selection ((t (:background ,zenburn-bg+1 :underline nil))))   `(helm-selection-line ((t (:background ,zenburn-bg+1))))   `(helm-visible-mark ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-2))))   `(helm-candidate-number ((t (:foreground ,zenburn-green+4 :background ,zenburn-bg-1))))   `(helm-separator ((t (:foreground ,zenburn-red :background ,zenburn-bg))))   `(helm-time-zone-current ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))   `(helm-time-zone-home ((t (:foreground ,zenburn-red :background ,zenburn-bg))))   `(helm-bookmark-addressbook ((t (:foreground ,zenburn-orange :background ,zenburn-bg))))   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))   `(helm-bookmark-gnus ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))   `(helm-bookmark-info ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))   `(helm-bookmark-man ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))))   `(helm-bookmark-w3m ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))   `(helm-buffer-not-saved ((t (:foreground ,zenburn-red :background ,zenburn-bg))))   `(helm-buffer-process ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))   `(helm-buffer-saved-out ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))   `(helm-buffer-size ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))   `(helm-ff-directory ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))   `(helm-ff-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg :weight normal))))   `(helm-ff-executable ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg :weight normal))))   `(helm-ff-invalid-symlink ((t (:foreground ,zenburn-red :background ,zenburn-bg :weight bold))))   `(helm-ff-symlink ((t (:foreground ,zenburn-yellow :background ,zenburn-bg :weight bold))))   `(helm-ff-prefix ((t (:foreground ,zenburn-bg :background ,zenburn-yellow :weight normal))))   `(helm-grep-cmd-line ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))   `(helm-grep-file ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))   `(helm-grep-finish ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))   `(helm-grep-lineno ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))   `(helm-grep-running ((t (:foreground ,zenburn-red :background ,zenburn-bg))))   `(helm-match ((t (:foreground ,zenburn-orange :background ,zenburn-bg-1 :weight bold))))   `(helm-moccur-buffer ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))))   `(helm-mu-contacts-address-face ((t (:foreground ,zenburn-fg-1 :background ,zenburn-bg))))   `(helm-mu-contacts-name-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg))));;;;; helm-swoop   `(helm-swoop-target-line-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+1))))   `(helm-swoop-target-word-face ((t (:foreground ,zenburn-yellow :background ,zenburn-bg+2 :weight bold))));;;;; hl-line-mode   `(hl-line-face ((,class (:background ,zenburn-bg-05))                   (t :weight bold)))   `(hl-line ((,class (:background ,zenburn-bg-05)) ; old emacsen              (t :weight bold)));;;;; hl-sexp   `(hl-sexp-face ((,class (:background ,zenburn-bg+1))                   (t :weight bold)));;;;; hydra   `(hydra-face-red ((t (:foreground ,zenburn-red-1 :background ,zenburn-bg))))   `(hydra-face-amaranth ((t (:foreground ,zenburn-red-3 :background ,zenburn-bg))))   `(hydra-face-blue ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))   `(hydra-face-pink ((t (:foreground ,zenburn-magenta :background ,zenburn-bg))))   `(hydra-face-teal ((t (:foreground ,zenburn-cyan :background ,zenburn-bg))));;;;; info+   `(info-command-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))   `(info-constant-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-magenta))))   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))   `(info-file ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))   `(info-function-ref-item ((t (:background ,zenburn-bg-1 :inherit font-lock-function-name-face))))   `(info-macro-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))   `(info-menu ((t (:foreground ,zenburn-yellow))))   `(info-quoted-name ((t (:inherit font-lock-constant-face))))   `(info-reference-item ((t (:background ,zenburn-bg-1))))   `(info-single-quote ((t (:inherit font-lock-keyword-face))))   `(info-special-form-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-yellow))))   `(info-string ((t (:inherit font-lock-string-face))))   `(info-syntax-class-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue+1))))   `(info-user-option-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-red))))   `(info-variable-ref-item ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))));;;;; irfc   `(irfc-head-name-face ((t (:foreground ,zenburn-red :weight bold))))   `(irfc-head-number-face ((t (:foreground ,zenburn-red :weight bold))))   `(irfc-reference-face ((t (:foreground ,zenburn-blue-1 :weight bold))))   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))   `(irfc-rfc-link-face ((t (:inherit link))))   `(irfc-rfc-number-face ((t (:foreground ,zenburn-cyan :weight bold))))   `(irfc-std-number-face ((t (:foreground ,zenburn-green+4 :weight bold))))   `(irfc-table-item-face ((t (:foreground ,zenburn-green+3))))   `(irfc-title-face ((t (:foreground ,zenburn-yellow                                      :underline t :weight bold))));;;;; ivy   `(ivy-confirm-face ((t (:foreground ,zenburn-green :background ,zenburn-bg))))   `(ivy-current-match ((t (:foreground ,zenburn-yellow :weight bold :underline t))))   `(ivy-cursor ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))   `(ivy-match-required-face ((t (:foreground ,zenburn-red :background ,zenburn-bg))))   `(ivy-minibuffer-match-face-1 ((t (:background ,zenburn-bg+1))))   `(ivy-minibuffer-match-face-2 ((t (:background ,zenburn-green-2))))   `(ivy-minibuffer-match-face-3 ((t (:background ,zenburn-green))))   `(ivy-minibuffer-match-face-4 ((t (:background ,zenburn-green+1))))   `(ivy-remote ((t (:foreground ,zenburn-blue :background ,zenburn-bg))))   `(ivy-subdir ((t (:foreground ,zenburn-yellow :background ,zenburn-bg))));;;;; ido-mode   `(ido-first-match ((t (:foreground ,zenburn-yellow :weight bold))))   `(ido-only-match ((t (:foreground ,zenburn-orange :weight bold))))   `(ido-subdir ((t (:foreground ,zenburn-yellow))))   `(ido-indicator ((t (:foreground ,zenburn-yellow :background ,zenburn-red-4))));;;;; iedit-mode   `(iedit-occurrence ((t (:background ,zenburn-bg+2 :weight bold))));;;;; jabber-mode   `(jabber-roster-user-away ((t (:foreground ,zenburn-green+2))))   `(jabber-roster-user-online ((t (:foreground ,zenburn-blue-1))))   `(jabber-roster-user-dnd ((t (:foreground ,zenburn-red+1))))   `(jabber-roster-user-xa ((t (:foreground ,zenburn-magenta))))   `(jabber-roster-user-chatty ((t (:foreground ,zenburn-orange))))   `(jabber-roster-user-error ((t (:foreground ,zenburn-red+1))))   `(jabber-rare-time-face ((t (:foreground ,zenburn-green+1))))   `(jabber-chat-prompt-local ((t (:foreground ,zenburn-blue-1))))   `(jabber-chat-prompt-foreign ((t (:foreground ,zenburn-red+1))))   `(jabber-chat-prompt-system ((t (:foreground ,zenburn-green+3))))   `(jabber-activity-face((t (:foreground ,zenburn-red+1))))   `(jabber-activity-personal-face ((t (:foreground ,zenburn-blue+1))))   `(jabber-title-small ((t (:height 1.1 :weight bold))))   `(jabber-title-medium ((t (:height 1.2 :weight bold))))   `(jabber-title-large ((t (:height 1.3 :weight bold))));;;;; js2-mode   `(js2-warning ((t (:underline ,zenburn-orange))))   `(js2-error ((t (:foreground ,zenburn-red :weight bold))))   `(js2-jsdoc-tag ((t (:foreground ,zenburn-green-2))))   `(js2-jsdoc-type ((t (:foreground ,zenburn-green+2))))   `(js2-jsdoc-value ((t (:foreground ,zenburn-green+3))))   `(js2-function-param ((t (:foreground, zenburn-orange))))   `(js2-external-variable ((t (:foreground ,zenburn-orange))));;;;; additional js2 mode attributes for better syntax highlighting   `(js2-instance-member ((t (:foreground ,zenburn-green-2))))   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,zenburn-orange))))   `(js2-jsdoc-html-tag-name ((t (:foreground ,zenburn-red-1))))   `(js2-object-property ((t (:foreground ,zenburn-blue+1))))   `(js2-magic-paren ((t (:foreground ,zenburn-blue-5))))   `(js2-private-function-call ((t (:foreground ,zenburn-cyan))))   `(js2-function-call ((t (:foreground ,zenburn-cyan))))   `(js2-private-member ((t (:foreground ,zenburn-blue-1))))   `(js2-keywords ((t (:foreground ,zenburn-magenta))));;;;; ledger-mode   `(ledger-font-payee-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))   `(ledger-font-payee-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))   `(ledger-font-payee-pending-face ((t (:foreground ,zenburn-red :weight normal))))   `(ledger-font-xact-highlight-face ((t (:background ,zenburn-bg+1))))   `(ledger-font-auto-xact-face ((t (:foreground ,zenburn-yellow-1 :weight normal))))   `(ledger-font-periodic-xact-face ((t (:foreground ,zenburn-green :weight normal))))   `(ledger-font-pending-face ((t (:foreground ,zenburn-orange weight: normal))))   `(ledger-font-other-face ((t (:foreground ,zenburn-fg))))   `(ledger-font-posting-date-face ((t (:foreground ,zenburn-orange :weight normal))))   `(ledger-font-posting-account-face ((t (:foreground ,zenburn-blue-1))))   `(ledger-font-posting-account-cleared-face ((t (:foreground ,zenburn-fg))))   `(ledger-font-posting-account-pending-face ((t (:foreground ,zenburn-orange))))   `(ledger-font-posting-amount-face ((t (:foreground ,zenburn-orange))))   `(ledger-occur-narrowed-face ((t (:foreground ,zenburn-fg-1 :invisible t))))   `(ledger-occur-xact-face ((t (:background ,zenburn-bg+1))))   `(ledger-font-comment-face ((t (:foreground ,zenburn-green))))   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,zenburn-red-1 :weight bold))))   `(ledger-font-reconciler-cleared-face ((t (:foreground ,zenburn-fg :weight normal))))   `(ledger-font-reconciler-pending-face ((t (:foreground ,zenburn-orange :weight normal))))   `(ledger-font-report-clickable-face ((t (:foreground ,zenburn-orange :weight normal))));;;;; linum-mode   `(linum ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))));;;;; lispy   `(lispy-command-name-face ((t (:background ,zenburn-bg-05 :inherit font-lock-function-name-face))))   `(lispy-cursor-face ((t (:foreground ,zenburn-bg :background ,zenburn-fg))))   `(lispy-face-hint ((t (:inherit highlight :foreground ,zenburn-yellow))));;;;; ruler-mode   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,zenburn-fg))))   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,zenburn-yellow))))   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))   `(ruler-mode-current-column ((t (:foreground ,zenburn-yellow :box t))))   `(ruler-mode-default ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))));;;;; lui   `(lui-time-stamp-face ((t (:foreground ,zenburn-blue-1))))   `(lui-hilight-face ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg))))   `(lui-button-face ((t (:inherit hover-highlight))));;;;; macrostep   `(macrostep-gensym-1     ((t (:foreground ,zenburn-green+2 :background ,zenburn-bg-1))))   `(macrostep-gensym-2     ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg-1))))   `(macrostep-gensym-3     ((t (:foreground ,zenburn-blue+1 :background ,zenburn-bg-1))))   `(macrostep-gensym-4     ((t (:foreground ,zenburn-magenta :background ,zenburn-bg-1))))   `(macrostep-gensym-5     ((t (:foreground ,zenburn-yellow :background ,zenburn-bg-1))))   `(macrostep-expansion-highlight-face     ((t (:inherit highlight))))   `(macrostep-macro-face     ((t (:underline t))));;;;; magit;;;;;; headings and diffs   `(magit-section-highlight           ((t (:background ,zenburn-bg+05))))   `(magit-section-heading             ((t (:foreground ,zenburn-yellow :weight bold))))   `(magit-section-heading-selection   ((t (:foreground ,zenburn-orange :weight bold))))   `(magit-diff-added ((t (:inherit diff-added))))   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))   `(magit-diff-removed ((t (:inherit diff-removed))))   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))   `(magit-diff-file-heading           ((t (:weight bold))))   `(magit-diff-file-heading-highlight ((t (:background ,zenburn-bg+05  :weight bold))))   `(magit-diff-file-heading-selection ((t (:background ,zenburn-bg+05                                                        :foreground ,zenburn-orange :weight bold))))   `(magit-diff-hunk-heading           ((t (:background ,zenburn-bg+1))))   `(magit-diff-hunk-heading-highlight ((t (:background ,zenburn-bg+2))))   `(magit-diff-hunk-heading-selection ((t (:background ,zenburn-bg+2                                                        :foreground ,zenburn-orange))))   `(magit-diff-lines-heading          ((t (:background ,zenburn-orange                                                        :foreground ,zenburn-bg+2))))   `(magit-diff-context-highlight      ((t (:background ,zenburn-bg+05                                                        :foreground "grey70"))))   `(magit-diffstat-added   ((t (:foreground ,zenburn-green+4))))   `(magit-diffstat-removed ((t (:foreground ,zenburn-red))));;;;;; popup   `(magit-popup-heading             ((t (:foreground ,zenburn-yellow  :weight bold))))   `(magit-popup-key                 ((t (:foreground ,zenburn-green-2 :weight bold))))   `(magit-popup-argument            ((t (:foreground ,zenburn-green   :weight bold))))   `(magit-popup-disabled-argument   ((t (:foreground ,zenburn-fg-1    :weight normal))))   `(magit-popup-option-value        ((t (:foreground ,zenburn-blue-2  :weight bold))));;;;;; process   `(magit-process-ok    ((t (:foreground ,zenburn-green  :weight bold))))   `(magit-process-ng    ((t (:foreground ,zenburn-red    :weight bold))));;;;;; log   `(magit-log-author    ((t (:foreground ,zenburn-orange))))   `(magit-log-date      ((t (:foreground ,zenburn-fg-1))))   `(magit-log-graph     ((t (:foreground ,zenburn-fg+1))));;;;;; sequence   `(magit-sequence-pick ((t (:foreground ,zenburn-yellow-2))))   `(magit-sequence-stop ((t (:foreground ,zenburn-green))))   `(magit-sequence-part ((t (:foreground ,zenburn-yellow))))   `(magit-sequence-head ((t (:foreground ,zenburn-blue))))   `(magit-sequence-drop ((t (:foreground ,zenburn-red))))   `(magit-sequence-done ((t (:foreground ,zenburn-fg-1))))   `(magit-sequence-onto ((t (:foreground ,zenburn-fg-1))));;;;;; bisect   `(magit-bisect-good ((t (:foreground ,zenburn-green))))   `(magit-bisect-skip ((t (:foreground ,zenburn-yellow))))   `(magit-bisect-bad  ((t (:foreground ,zenburn-red))));;;;;; blame   `(magit-blame-heading ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))   `(magit-blame-hash    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2))))   `(magit-blame-name    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))   `(magit-blame-date    ((t (:background ,zenburn-bg-1 :foreground ,zenburn-orange))))   `(magit-blame-summary ((t (:background ,zenburn-bg-1 :foreground ,zenburn-blue-2                                          :weight bold))));;;;;; references etc   `(magit-dimmed         ((t (:foreground ,zenburn-bg+3))))   `(magit-hash           ((t (:foreground ,zenburn-bg+3))))   `(magit-tag            ((t (:foreground ,zenburn-orange :weight bold))))   `(magit-branch-remote  ((t (:foreground ,zenburn-green  :weight bold))))   `(magit-branch-local   ((t (:foreground ,zenburn-blue   :weight bold))))   `(magit-branch-current ((t (:foreground ,zenburn-blue   :weight bold :box t))))   `(magit-head           ((t (:foreground ,zenburn-blue   :weight bold))))   `(magit-refname        ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))   `(magit-refname-stash  ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))   `(magit-refname-wip    ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg :weight bold))))   `(magit-signature-good      ((t (:foreground ,zenburn-green))))   `(magit-signature-bad       ((t (:foreground ,zenburn-red))))   `(magit-signature-untrusted ((t (:foreground ,zenburn-yellow))))   `(magit-signature-expired   ((t (:foreground ,zenburn-orange))))   `(magit-signature-revoked   ((t (:foreground ,zenburn-magenta))))   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))   `(magit-cherry-unmatched    ((t (:foreground ,zenburn-cyan))))   `(magit-cherry-equivalent   ((t (:foreground ,zenburn-magenta))))   `(magit-reflog-commit       ((t (:foreground ,zenburn-green))))   `(magit-reflog-amend        ((t (:foreground ,zenburn-magenta))))   `(magit-reflog-merge        ((t (:foreground ,zenburn-green))))   `(magit-reflog-checkout     ((t (:foreground ,zenburn-blue))))   `(magit-reflog-reset        ((t (:foreground ,zenburn-red))))   `(magit-reflog-rebase       ((t (:foreground ,zenburn-magenta))))   `(magit-reflog-cherry-pick  ((t (:foreground ,zenburn-green))))   `(magit-reflog-remote       ((t (:foreground ,zenburn-cyan))))   `(magit-reflog-other        ((t (:foreground ,zenburn-cyan))));;;;; markup-faces   `(markup-anchor-face ((t (:foreground ,zenburn-blue+1))))   `(markup-code-face ((t (:inherit font-lock-constant-face))))   `(markup-command-face ((t (:foreground ,zenburn-yellow))))   `(markup-emphasis-face ((t (:inherit bold))))   `(markup-internal-reference-face ((t (:foreground ,zenburn-yellow-2 :underline t))))   `(markup-list-face ((t (:foreground ,zenburn-fg+1))))   `(markup-meta-face ((t (:foreground ,zenburn-yellow))))   `(markup-meta-hide-face ((t (:foreground ,zenburn-yellow))))   `(markup-secondary-text-face ((t (:foreground ,zenburn-yellow-1))))   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))   `(markup-value-face ((t (:foreground ,zenburn-yellow))));;;;; message-mode   `(message-cited-text ((t (:inherit font-lock-comment-face))))   `(message-header-name ((t (:foreground ,zenburn-green+1))))   `(message-header-other ((t (:foreground ,zenburn-green))))   `(message-header-to ((t (:foreground ,zenburn-yellow :weight bold))))   `(message-header-cc ((t (:foreground ,zenburn-yellow :weight bold))))   `(message-header-newsgroups ((t (:foreground ,zenburn-yellow :weight bold))))   `(message-header-subject ((t (:foreground ,zenburn-orange :weight bold))))   `(message-header-xheader ((t (:foreground ,zenburn-green))))   `(message-mml ((t (:foreground ,zenburn-yellow :weight bold))))   `(message-separator ((t (:inherit font-lock-comment-face))));;;;; mew   `(mew-face-header-subject ((t (:foreground ,zenburn-orange))))   `(mew-face-header-from ((t (:foreground ,zenburn-yellow))))   `(mew-face-header-date ((t (:foreground ,zenburn-green))))   `(mew-face-header-to ((t (:foreground ,zenburn-red))))   `(mew-face-header-key ((t (:foreground ,zenburn-green))))   `(mew-face-header-private ((t (:foreground ,zenburn-green))))   `(mew-face-header-important ((t (:foreground ,zenburn-blue))))   `(mew-face-header-marginal ((t (:foreground ,zenburn-fg :weight bold))))   `(mew-face-header-warning ((t (:foreground ,zenburn-red))))   `(mew-face-header-xmew ((t (:foreground ,zenburn-green))))   `(mew-face-header-xmew-bad ((t (:foreground ,zenburn-red))))   `(mew-face-body-url ((t (:foreground ,zenburn-orange))))   `(mew-face-body-comment ((t (:foreground ,zenburn-fg :slant italic))))   `(mew-face-body-cite1 ((t (:foreground ,zenburn-green))))   `(mew-face-body-cite2 ((t (:foreground ,zenburn-blue))))   `(mew-face-body-cite3 ((t (:foreground ,zenburn-orange))))   `(mew-face-body-cite4 ((t (:foreground ,zenburn-yellow))))   `(mew-face-body-cite5 ((t (:foreground ,zenburn-red))))   `(mew-face-mark-review ((t (:foreground ,zenburn-blue))))   `(mew-face-mark-escape ((t (:foreground ,zenburn-green))))   `(mew-face-mark-delete ((t (:foreground ,zenburn-red))))   `(mew-face-mark-unlink ((t (:foreground ,zenburn-yellow))))   `(mew-face-mark-refile ((t (:foreground ,zenburn-green))))   `(mew-face-mark-unread ((t (:foreground ,zenburn-red-2))))   `(mew-face-eof-message ((t (:foreground ,zenburn-green))))   `(mew-face-eof-part ((t (:foreground ,zenburn-yellow))));;;;; mic-paren   `(paren-face-match ((t (:foreground ,zenburn-cyan :background ,zenburn-bg :weight bold))))   `(paren-face-mismatch ((t (:foreground ,zenburn-bg :background ,zenburn-magenta :weight bold))))   `(paren-face-no-match ((t (:foreground ,zenburn-bg :background ,zenburn-red :weight bold))));;;;; mingus   `(mingus-directory-face ((t (:foreground ,zenburn-blue))))   `(mingus-pausing-face ((t (:foreground ,zenburn-magenta))))   `(mingus-playing-face ((t (:foreground ,zenburn-cyan))))   `(mingus-playlist-face ((t (:foreground ,zenburn-cyan ))))   `(mingus-mark-face ((t (:bold t :foreground ,zenburn-magenta))))   `(mingus-song-file-face ((t (:foreground ,zenburn-yellow))))   `(mingus-artist-face ((t (:foreground ,zenburn-cyan))))   `(mingus-album-face ((t (:underline t :foreground ,zenburn-red+1))))   `(mingus-album-stale-face ((t (:foreground ,zenburn-red+1))))   `(mingus-stopped-face ((t (:foreground ,zenburn-red))));;;;; nav   `(nav-face-heading ((t (:foreground ,zenburn-yellow))))   `(nav-face-button-num ((t (:foreground ,zenburn-cyan))))   `(nav-face-dir ((t (:foreground ,zenburn-green))))   `(nav-face-hdir ((t (:foreground ,zenburn-red))))   `(nav-face-file ((t (:foreground ,zenburn-fg))))   `(nav-face-hfile ((t (:foreground ,zenburn-red-4))));;;;; merlin   `(merlin-type-face ((t (:inherit highlight))))   `(merlin-compilation-warning-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-orange)))      (t       (:underline ,zenburn-orange))))   `(merlin-compilation-error-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-red)))      (t       (:underline ,zenburn-red))));;;;; mu4e   `(mu4e-cited-1-face ((t (:foreground ,zenburn-blue    :slant italic))))   `(mu4e-cited-2-face ((t (:foreground ,zenburn-green+2 :slant italic))))   `(mu4e-cited-3-face ((t (:foreground ,zenburn-blue-2  :slant italic))))   `(mu4e-cited-4-face ((t (:foreground ,zenburn-green   :slant italic))))   `(mu4e-cited-5-face ((t (:foreground ,zenburn-blue-4  :slant italic))))   `(mu4e-cited-6-face ((t (:foreground ,zenburn-green-2 :slant italic))))   `(mu4e-cited-7-face ((t (:foreground ,zenburn-blue    :slant italic))))   `(mu4e-replied-face ((t (:foreground ,zenburn-bg+3))))   `(mu4e-trashed-face ((t (:foreground ,zenburn-bg+3 :strike-through t))));;;;; mumamo   `(mumamo-background-chunk-major ((t (:background nil))))   `(mumamo-background-chunk-submode1 ((t (:background ,zenburn-bg-1))))   `(mumamo-background-chunk-submode2 ((t (:background ,zenburn-bg+2))))   `(mumamo-background-chunk-submode3 ((t (:background ,zenburn-bg+3))))   `(mumamo-background-chunk-submode4 ((t (:background ,zenburn-bg+1))));;;;; neotree   `(neo-banner-face ((t (:foreground ,zenburn-blue+1 :weight bold))))   `(neo-header-face ((t (:foreground ,zenburn-fg))))   `(neo-root-dir-face ((t (:foreground ,zenburn-blue+1 :weight bold))))   `(neo-dir-link-face ((t (:foreground ,zenburn-blue))))   `(neo-file-link-face ((t (:foreground ,zenburn-fg))))   `(neo-expand-btn-face ((t (:foreground ,zenburn-blue))))   `(neo-vc-default-face ((t (:foreground ,zenburn-fg+1))))   `(neo-vc-user-face ((t (:foreground ,zenburn-red :slant italic))))   `(neo-vc-up-to-date-face ((t (:foreground ,zenburn-fg))))   `(neo-vc-edited-face ((t (:foreground ,zenburn-magenta))))   `(neo-vc-needs-merge-face ((t (:foreground ,zenburn-red+1))))   `(neo-vc-unlocked-changes-face ((t (:foreground ,zenburn-red :background ,zenburn-blue-5))))   `(neo-vc-added-face ((t (:foreground ,zenburn-green+1))))   `(neo-vc-conflict-face ((t (:foreground ,zenburn-red+1))))   `(neo-vc-missing-face ((t (:foreground ,zenburn-red+1))))   `(neo-vc-ignored-face ((t (:foreground ,zenburn-fg-1))));;;;; org-mode   `(org-agenda-date-today     ((t (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)   `(org-agenda-structure     ((t (:inherit font-lock-comment-face))))   `(org-archived ((t (:foreground ,zenburn-fg :weight bold))))   `(org-checkbox ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1                                   :box (:line-width 1 :style released-button)))))   `(org-date ((t (:foreground ,zenburn-blue :underline t))))   `(org-deadline-announce ((t (:foreground ,zenburn-red-1))))   `(org-done ((t (:weight bold :weight bold :foreground ,zenburn-green+3))))   `(org-formula ((t (:foreground ,zenburn-yellow-2))))   `(org-headline-done ((t (:foreground ,zenburn-green+3))))   `(org-hide ((t (:foreground ,zenburn-bg))))   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange                               ,@(when zenburn-scale-org-headlines                                   (list :height zenburn-height-plus-4))))))   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4                               ,@(when zenburn-scale-org-headlines                                   (list :height zenburn-height-plus-3))))))   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1                               ,@(when zenburn-scale-org-headlines                                   (list :height zenburn-height-plus-2))))))   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2                               ,@(when zenburn-scale-org-headlines                                   (list :height zenburn-height-plus-1))))))   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))))   `(org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))   `(org-scheduled ((t (:foreground ,zenburn-green+4))))   `(org-scheduled-previously ((t (:foreground ,zenburn-red))))   `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))   `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))   `(org-special-keyword ((t (:inherit font-lock-comment-face))))   `(org-table ((t (:foreground ,zenburn-green+2))))   `(org-tag ((t (:weight bold :weight bold))))   `(org-time-grid ((t (:foreground ,zenburn-orange))))   `(org-todo ((t (:weight bold :foreground ,zenburn-red :weight bold))))   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))   `(org-warning ((t (:weight bold :foreground ,zenburn-red :weight bold :underline nil))))   `(org-column ((t (:background ,zenburn-bg-1))))   `(org-column-title ((t (:background ,zenburn-bg-1 :underline t :weight bold))))   `(org-mode-line-clock ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))   `(org-mode-line-clock-overrun ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))   `(org-ellipsis ((t (:foreground ,zenburn-yellow-1 :underline t))))   `(org-footnote ((t (:foreground ,zenburn-cyan :underline t))))   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue                                         :weight bold :height ,zenburn-height-plus-4))))   `(org-document-info ((t (:foreground ,zenburn-blue))))   `(org-habit-ready-face ((t :background ,zenburn-green)))   `(org-habit-alert-face ((t :background ,zenburn-yellow-1 :foreground ,zenburn-bg)))   `(org-habit-clear-face ((t :background ,zenburn-blue-3)))   `(org-habit-overdue-face ((t :background ,zenburn-red-3)))   `(org-habit-clear-future-face ((t :background ,zenburn-blue-4)))   `(org-habit-ready-future-face ((t :background ,zenburn-green-2)))   `(org-habit-alert-future-face ((t :background ,zenburn-yellow-2 :foreground ,zenburn-bg)))   `(org-habit-overdue-future-face ((t :background ,zenburn-red-4)));;;;; org-ref   `(org-ref-ref-face ((t :underline t)))   `(org-ref-label-face ((t :underline t)))   `(org-ref-cite-face ((t :underline t)))   `(org-ref-glossary-face ((t :underline t)))   `(org-ref-acronym-face ((t :underline t)));;;;; outline   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-orange                             ,@(when zenburn-scale-outline-headlines                                 (list :height zenburn-height-plus-4))))))   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+4                             ,@(when zenburn-scale-outline-headlines                                 (list :height zenburn-height-plus-3))))))   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-1                             ,@(when zenburn-scale-outline-headlines                                 (list :height zenburn-height-plus-2))))))   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-yellow-2                             ,@(when zenburn-scale-outline-headlines                                 (list :height zenburn-height-plus-1))))))   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-cyan))))   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-green+2))))   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-red-4))))   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,zenburn-blue-4))));;;;; p4   `(p4-depot-added-face ((t :inherit diff-added)))   `(p4-depot-branch-op-face ((t :inherit diff-changed)))   `(p4-depot-deleted-face ((t :inherit diff-removed)))   `(p4-depot-unmapped-face ((t :inherit diff-changed)))   `(p4-diff-change-face ((t :inherit diff-changed)))   `(p4-diff-del-face ((t :inherit diff-removed)))   `(p4-diff-file-face ((t :inherit diff-file-header)))   `(p4-diff-head-face ((t :inherit diff-header)))   `(p4-diff-ins-face ((t :inherit diff-added)));;;;; c/perl   `(cperl-nonoverridable-face ((t (:foreground ,zenburn-magenta))))   `(cperl-array-face ((t (:foreground ,zenburn-yellow, :backgorund ,zenburn-bg))))   `(cperl-hash-face ((t (:foreground ,zenburn-yellow-1, :background ,zenburn-bg))));;;;; paren-face   `(parenthesis ((t (:foreground ,zenburn-fg-1))));;;;; perspective   `(persp-selected-face ((t (:foreground ,zenburn-yellow-2 :inherit mode-line))));;;;; powerline   `(powerline-active1 ((t (:background ,zenburn-bg-05 :inherit mode-line))))   `(powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line))))   `(powerline-inactive1 ((t (:background ,zenburn-bg+1 :inherit mode-line-inactive))))   `(powerline-inactive2 ((t (:background ,zenburn-bg+3 :inherit mode-line-inactive))));;;;; proofgeneral   `(proof-active-area-face ((t (:underline t))))   `(proof-boring-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg+2))))   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))   `(proof-debug-message-face ((t (:inherit proof-boring-face))))   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))   `(proof-eager-annotation-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))   `(proof-error-face ((t (:foreground ,zenburn-fg :background ,zenburn-red-4))))   `(proof-highlight-dependency-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))))   `(proof-highlight-dependent-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))   `(proof-locked-face ((t (:background ,zenburn-blue-5))))   `(proof-mouse-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))   `(proof-queue-face ((t (:background ,zenburn-red-4))))   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))   `(proof-script-highlight-error-face ((t (:background ,zenburn-red-2))))   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,zenburn-bg))))   `(proof-warning-face ((t (:foreground ,zenburn-bg :background ,zenburn-yellow-1))));;;;; racket-mode   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))   `(racket-selfeval-face ((t (:inherit font-lock-type-face))));;;;; rainbow-delimiters   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenburn-fg))))   `(rainbow-delimiters-depth-2-face ((t (:foreground ,zenburn-green+4))))   `(rainbow-delimiters-depth-3-face ((t (:foreground ,zenburn-yellow-2))))   `(rainbow-delimiters-depth-4-face ((t (:foreground ,zenburn-cyan))))   `(rainbow-delimiters-depth-5-face ((t (:foreground ,zenburn-green+2))))   `(rainbow-delimiters-depth-6-face ((t (:foreground ,zenburn-blue+1))))   `(rainbow-delimiters-depth-7-face ((t (:foreground ,zenburn-yellow-1))))   `(rainbow-delimiters-depth-8-face ((t (:foreground ,zenburn-green+1))))   `(rainbow-delimiters-depth-9-face ((t (:foreground ,zenburn-blue-2))))   `(rainbow-delimiters-depth-10-face ((t (:foreground ,zenburn-orange))))   `(rainbow-delimiters-depth-11-face ((t (:foreground ,zenburn-green))))   `(rainbow-delimiters-depth-12-face ((t (:foreground ,zenburn-blue-5))));;;;; rcirc   `(rcirc-my-nick ((t (:foreground ,zenburn-blue))))   `(rcirc-other-nick ((t (:foreground ,zenburn-orange))))   `(rcirc-bright-nick ((t (:foreground ,zenburn-blue+1))))   `(rcirc-dim-nick ((t (:foreground ,zenburn-blue-2))))   `(rcirc-server ((t (:foreground ,zenburn-green))))   `(rcirc-server-prefix ((t (:foreground ,zenburn-green+1))))   `(rcirc-timestamp ((t (:foreground ,zenburn-green+2))))   `(rcirc-nick-in-message ((t (:foreground ,zenburn-yellow))))   `(rcirc-nick-in-message-full-line ((t (:weight bold))))   `(rcirc-prompt ((t (:foreground ,zenburn-yellow :weight bold))))   `(rcirc-track-nick ((t (:inverse-video t))))   `(rcirc-track-keyword ((t (:weight bold))))   `(rcirc-url ((t (:weight bold))))   `(rcirc-keyword ((t (:foreground ,zenburn-yellow :weight bold))));;;;; re-builder   `(reb-match-0 ((t (:foreground ,zenburn-bg :background ,zenburn-magenta))))   `(reb-match-1 ((t (:foreground ,zenburn-bg :background ,zenburn-blue))))   `(reb-match-2 ((t (:foreground ,zenburn-bg :background ,zenburn-orange))))   `(reb-match-3 ((t (:foreground ,zenburn-bg :background ,zenburn-red))));;;;; realgud   `(realgud-overlay-arrow1 ((t (:foreground ,zenburn-green))))   `(realgud-overlay-arrow2 ((t (:foreground ,zenburn-yellow))))   `(realgud-overlay-arrow3 ((t (:foreground ,zenburn-orange))))   `(realgud-bp-enabled-face ((t (:inherit error))))   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))   `(realgud-bp-line-enabled-face ((t (:box (:color ,zenburn-red :style nil)))))   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))   `(realgud-line-number ((t (:foreground ,zenburn-yellow))))   `(realgud-backtrace-number ((t (:foreground ,zenburn-yellow, :weight bold))));;;;; regex-tool   `(regex-tool-matched-face ((t (:background ,zenburn-blue-4 :weight bold))));;;;; rpm-mode   `(rpm-spec-dir-face ((t (:foreground ,zenburn-green))))   `(rpm-spec-doc-face ((t (:foreground ,zenburn-green))))   `(rpm-spec-ghost-face ((t (:foreground ,zenburn-red))))   `(rpm-spec-macro-face ((t (:foreground ,zenburn-yellow))))   `(rpm-spec-obsolete-tag-face ((t (:foreground ,zenburn-red))))   `(rpm-spec-package-face ((t (:foreground ,zenburn-red))))   `(rpm-spec-section-face ((t (:foreground ,zenburn-yellow))))   `(rpm-spec-tag-face ((t (:foreground ,zenburn-blue))))   `(rpm-spec-var-face ((t (:foreground ,zenburn-red))));;;;; rst-mode   `(rst-level-1-face ((t (:foreground ,zenburn-orange))))   `(rst-level-2-face ((t (:foreground ,zenburn-green+1))))   `(rst-level-3-face ((t (:foreground ,zenburn-blue-1))))   `(rst-level-4-face ((t (:foreground ,zenburn-yellow-2))))   `(rst-level-5-face ((t (:foreground ,zenburn-cyan))))   `(rst-level-6-face ((t (:foreground ,zenburn-green-2))));;;;; sh-mode   `(sh-heredoc     ((t (:foreground ,zenburn-yellow :weight bold))))   `(sh-quoted-exec ((t (:foreground ,zenburn-red))));;;;; show-paren   `(show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))   `(show-paren-match ((t (:foreground ,zenburn-fg :background ,zenburn-bg+3 :weight bold))));;;;; smart-mode-line   ;; use (setq sml/theme nil) to enable Zenburn for sml   `(sml/global ((,class (:foreground ,zenburn-fg :weight bold))))   `(sml/modes ((,class (:foreground ,zenburn-yellow :weight bold))))   `(sml/minor-modes ((,class (:foreground ,zenburn-fg-1 :weight bold))))   `(sml/filename ((,class (:foreground ,zenburn-yellow :weight bold))))   `(sml/line-number ((,class (:foreground ,zenburn-blue :weight bold))))   `(sml/col-number ((,class (:foreground ,zenburn-blue+1 :weight bold))))   `(sml/position-percentage ((,class (:foreground ,zenburn-blue-1 :weight bold))))   `(sml/prefix ((,class (:foreground ,zenburn-orange))))   `(sml/git ((,class (:foreground ,zenburn-green+3))))   `(sml/process ((,class (:weight bold))))   `(sml/sudo ((,class  (:foreground ,zenburn-orange :weight bold))))   `(sml/read-only ((,class (:foreground ,zenburn-red-2))))   `(sml/outside-modified ((,class (:foreground ,zenburn-orange))))   `(sml/modified ((,class (:foreground ,zenburn-red))))   `(sml/vc-edited ((,class (:foreground ,zenburn-green+2))))   `(sml/charging ((,class (:foreground ,zenburn-green+4))))   `(sml/discharging ((,class (:foreground ,zenburn-red+1))));;;;; smartparens   `(sp-show-pair-mismatch-face ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3 :weight bold))))   `(sp-show-pair-match-face ((t (:background ,zenburn-bg+3 :weight bold))));;;;; sml-mode-line   '(sml-modeline-end-face ((t :inherit default :width condensed)));;;;; SLIME   `(slime-repl-output-face ((t (:foreground ,zenburn-red))))   `(slime-repl-inputed-output-face ((t (:foreground ,zenburn-green))))   `(slime-error-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-red)))      (t       (:underline ,zenburn-red))))   `(slime-warning-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-orange)))      (t       (:underline ,zenburn-orange))))   `(slime-style-warning-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-yellow)))      (t       (:underline ,zenburn-yellow))))   `(slime-note-face     ((((supports :underline (:style wave)))       (:underline (:style wave :color ,zenburn-green)))      (t       (:underline ,zenburn-green))))   `(slime-highlight-face ((t (:inherit highlight))));;;;; speedbar   `(speedbar-button-face ((t (:foreground ,zenburn-green+2))))   `(speedbar-directory-face ((t (:foreground ,zenburn-cyan))))   `(speedbar-file-face ((t (:foreground ,zenburn-fg))))   `(speedbar-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-green+2))))   `(speedbar-selected-face ((t (:foreground ,zenburn-red))))   `(speedbar-separator-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-1))))   `(speedbar-tag-face ((t (:foreground ,zenburn-yellow))));;;;; sx   `(sx-custom-button     ((t (:background ,zenburn-fg :foreground ,zenburn-bg-1          :box (:line-width 3 :style released-button) :height 0.9))))   `(sx-question-list-answers     ((t (:foreground ,zenburn-green+3          :height 1.0 :inherit sx-question-list-parent))))   `(sx-question-mode-accepted     ((t (:foreground ,zenburn-green+3          :height 1.3 :inherit sx-question-mode-title))))   '(sx-question-mode-content-face ((t (:inherit highlight))))   `(sx-question-mode-kbd-tag     ((t (:box (:color ,zenburn-bg-1 :line-width 3 :style released-button)          :height 0.9 :weight semi-bold))));;;;; tabbar   `(tabbar-button ((t (:foreground ,zenburn-fg                                    :background ,zenburn-bg))))   `(tabbar-selected ((t (:foreground ,zenburn-fg                                      :background ,zenburn-bg                                      :box (:line-width -1 :style pressed-button)))))   `(tabbar-unselected ((t (:foreground ,zenburn-fg                                        :background ,zenburn-bg+1                                        :box (:line-width -1 :style released-button)))));;;;; term   `(term-color-black ((t (:foreground ,zenburn-bg                                       :background ,zenburn-bg-1))))   `(term-color-red ((t (:foreground ,zenburn-red-2                                     :background ,zenburn-red-4))))   `(term-color-green ((t (:foreground ,zenburn-green                                       :background ,zenburn-green+2))))   `(term-color-yellow ((t (:foreground ,zenburn-orange                                        :background ,zenburn-yellow))))   `(term-color-blue ((t (:foreground ,zenburn-blue-1                                      :background ,zenburn-blue-4))))   `(term-color-magenta ((t (:foreground ,zenburn-magenta                                         :background ,zenburn-red))))   `(term-color-cyan ((t (:foreground ,zenburn-cyan                                      :background ,zenburn-blue))))   `(term-color-white ((t (:foreground ,zenburn-fg                                       :background ,zenburn-fg-1))))   '(term-default-fg-color ((t (:inherit term-color-white))))   '(term-default-bg-color ((t (:inherit term-color-black))));;;;; undo-tree   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,zenburn-fg+1 :weight bold))))   `(undo-tree-visualizer-current-face ((t (:foreground ,zenburn-red-1 :weight bold))))   `(undo-tree-visualizer-default-face ((t (:foreground ,zenburn-fg))))   `(undo-tree-visualizer-register-face ((t (:foreground ,zenburn-yellow))))   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,zenburn-cyan))));;;;; visual-regexp   `(vr/group-0 ((t (:foreground ,zenburn-bg :background ,zenburn-green :weight bold))))   `(vr/group-1 ((t (:foreground ,zenburn-bg :background ,zenburn-orange :weight bold))))   `(vr/group-2 ((t (:foreground ,zenburn-bg :background ,zenburn-blue :weight bold))))   `(vr/match-0 ((t (:inherit isearch))))   `(vr/match-1 ((t (:foreground ,zenburn-yellow-2 :background ,zenburn-bg-1 :weight bold))))   `(vr/match-separator-face ((t (:foreground ,zenburn-red :weight bold))));;;;; volatile-highlights   `(vhl/default-face ((t (:background ,zenburn-bg-05))));;;;; web-mode   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))   `(web-mode-css-at-rule-face ((t (:foreground ,zenburn-orange ))))   `(web-mode-css-prop-face ((t (:foreground ,zenburn-orange))))   `(web-mode-css-pseudo-class-face ((t (:foreground ,zenburn-green+3 :weight bold))))   `(web-mode-css-rule-face ((t (:foreground ,zenburn-blue))))   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))   `(web-mode-folded-face ((t (:underline t))))   `(web-mode-function-name-face ((t (:foreground ,zenburn-blue))))   `(web-mode-html-attr-name-face ((t (:foreground ,zenburn-orange))))   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))   `(web-mode-html-tag-face ((t (:foreground ,zenburn-cyan))))   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))   `(web-mode-server-background-face ((t (:background ,zenburn-bg))))   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))   `(web-mode-whitespaces-face ((t (:background ,zenburn-red))));;;;; whitespace-mode   `(whitespace-space ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))   `(whitespace-hspace ((t (:background ,zenburn-bg+1 :foreground ,zenburn-bg+1))))   `(whitespace-tab ((t (:background ,zenburn-red-1))))   `(whitespace-newline ((t (:foreground ,zenburn-bg+1))))   `(whitespace-trailing ((t (:background ,zenburn-red))))   `(whitespace-line ((t (:background ,zenburn-bg :foreground ,zenburn-magenta))))   `(whitespace-space-before-tab ((t (:background ,zenburn-orange :foreground ,zenburn-orange))))   `(whitespace-indentation ((t (:background ,zenburn-yellow :foreground ,zenburn-red))))   `(whitespace-empty ((t (:background ,zenburn-yellow))))   `(whitespace-space-after-tab ((t (:background ,zenburn-yellow :foreground ,zenburn-red))));;;;; wanderlust   `(wl-highlight-folder-few-face ((t (:foreground ,zenburn-red-2))))   `(wl-highlight-folder-many-face ((t (:foreground ,zenburn-red-1))))   `(wl-highlight-folder-path-face ((t (:foreground ,zenburn-orange))))   `(wl-highlight-folder-unread-face ((t (:foreground ,zenburn-blue))))   `(wl-highlight-folder-zero-face ((t (:foreground ,zenburn-fg))))   `(wl-highlight-folder-unknown-face ((t (:foreground ,zenburn-blue))))   `(wl-highlight-message-citation-header ((t (:foreground ,zenburn-red-1))))   `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburn-red))))   `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburn-green+2))))   `(wl-highlight-message-cited-text-3 ((t (:foreground ,zenburn-blue))))   `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburn-blue+1))))   `(wl-highlight-message-header-contents-face ((t (:foreground ,zenburn-green))))   `(wl-highlight-message-headers-face ((t (:foreground ,zenburn-red+1))))   `(wl-highlight-message-important-header-contents ((t (:foreground ,zenburn-green+2))))   `(wl-highlight-message-header-contents ((t (:foreground ,zenburn-green+1))))   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,zenburn-green+2))))   `(wl-highlight-message-signature ((t (:foreground ,zenburn-green))))   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,zenburn-fg))))   `(wl-highlight-summary-answered-face ((t (:foreground ,zenburn-blue))))   `(wl-highlight-summary-disposed-face ((t (:foreground ,zenburn-fg                                                         :slant italic))))   `(wl-highlight-summary-new-face ((t (:foreground ,zenburn-blue))))   `(wl-highlight-summary-normal-face ((t (:foreground ,zenburn-fg))))   `(wl-highlight-summary-thread-top-face ((t (:foreground ,zenburn-yellow))))   `(wl-highlight-thread-indent-face ((t (:foreground ,zenburn-magenta))))   `(wl-highlight-summary-refiled-face ((t (:foreground ,zenburn-fg))))   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))));;;;; which-func-mode   `(which-func ((t (:foreground ,zenburn-green+4))));;;;; xcscope   `(cscope-file-face ((t (:foreground ,zenburn-yellow :weight bold))))   `(cscope-function-face ((t (:foreground ,zenburn-cyan :weight bold))))   `(cscope-line-number-face ((t (:foreground ,zenburn-red :weight bold))))   `(cscope-mouse-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue+1))))   `(cscope-separator-face ((t (:foreground ,zenburn-red :weight bold                                            :underline t :overline t))));;;;; yascroll   `(yascroll:thumb-text-area ((t (:background ,zenburn-bg-1))))   `(yascroll:thumb-fringe ((t (:background ,zenburn-bg-1 :foreground ,zenburn-bg-1))))   ));;; Theme Variables(zenburn-with-color-variables  (custom-theme-set-variables   'zenburn;;;;; ansi-color   `(ansi-color-names-vector [,zenburn-bg ,zenburn-red ,zenburn-green ,zenburn-yellow                                          ,zenburn-blue ,zenburn-magenta ,zenburn-cyan ,zenburn-fg]);;;;; company-quickhelp   `(company-quickhelp-color-background ,zenburn-bg+1)   `(company-quickhelp-color-foreground ,zenburn-fg);;;;; fill-column-indicator   `(fci-rule-color ,zenburn-bg-05);;;;; nrepl-client   `(nrepl-message-colors     '(,zenburn-red ,zenburn-orange ,zenburn-yellow ,zenburn-green ,zenburn-green+4       ,zenburn-cyan ,zenburn-blue+1 ,zenburn-magenta));;;;; pdf-tools   `(pdf-view-midnight-colors '(,zenburn-fg . ,zenburn-bg-05));;;;; vc-annotate   `(vc-annotate-color-map     '(( 20. . ,zenburn-red-1)       ( 40. . ,zenburn-red)       ( 60. . ,zenburn-orange)       ( 80. . ,zenburn-yellow-2)       (100. . ,zenburn-yellow-1)       (120. . ,zenburn-yellow)       (140. . ,zenburn-green-2)       (160. . ,zenburn-green)       (180. . ,zenburn-green+1)       (200. . ,zenburn-green+2)       (220. . ,zenburn-green+3)       (240. . ,zenburn-green+4)       (260. . ,zenburn-cyan)       (280. . ,zenburn-blue-2)       (300. . ,zenburn-blue-1)       (320. . ,zenburn-blue)       (340. . ,zenburn-blue+1)       (360. . ,zenburn-magenta)))   `(vc-annotate-very-old-color ,zenburn-magenta)   `(vc-annotate-background ,zenburn-bg-1)   ));;; Rainbow Support(declare-function rainbow-mode 'rainbow-mode)(declare-function rainbow-colorize-by-assoc 'rainbow-mode)(defvar zenburn-add-font-lock-keywords nil  "Whether to add font-lock keywords for zenburn color names.In buffers visiting library `zenburn-theme.el' the zenburnspecific keywords are always added.  In all other Emacs-Lispbuffers this variable controls whether this should be done.This requires library `rainbow-mode'.")(defvar zenburn-colors-font-lock-keywords nil);; (defadvice rainbow-turn-on (after zenburn activate);;   "Maybe also add font-lock keywords for zenburn colors.";;   (when (and (derived-mode-p 'emacs-lisp-mode);;              (or zenburn-add-font-lock-keywords;;                  (equal (file-name-nondirectory (buffer-file-name));;                         "zenburn-theme.el")));;     (unless zenburn-colors-font-lock-keywords;;       (setq zenburn-colors-font-lock-keywords;;             `((,(regexp-opt (mapcar 'car zenburn-colors-alist) 'words);;                (0 (rainbow-colorize-by-assoc zenburn-colors-alist))))));;     (font-lock-add-keywords nil zenburn-colors-font-lock-keywords)));; (defadvice rainbow-turn-off (after zenburn activate);;   "Also remove font-lock keywords for zenburn colors.";;   (font-lock-remove-keywords nil zenburn-colors-font-lock-keywords));;; Footer;;;###autoload(and load-file-name     (boundp 'custom-theme-load-path)     (add-to-list 'custom-theme-load-path                  (file-name-as-directory                   (file-name-directory load-file-name))))(provide-theme 'zenburn);; Local Variables:;; no-byte-compile: t;; indent-tabs-mode: nil;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1));; End:;;; zenburn-theme.el ends here               7   P   \   � �E G) ��                          "�