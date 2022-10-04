{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.Exit (exitFailure)
import           System.IO

import SDL (($=))
import qualified SDL

import qualified Graphics.GL as GL
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Raw as SDL.Scancode
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Foreign
import qualified SDL.Internal.Exception as SDL
import Graphics.Rendering.OpenGL (shaderInfoLog)
import qualified Data.Text
import Foreign.C (newCString)
import qualified Control.Monad.Zip as V

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data VertexArrayObject = VertexArrayObject
  {
    vertices:: V.Vector Float
  , colors:: V.Vector Float
  } deriving Show

vaoToVector:: VertexArrayObject -> V.Vector Float
vaoToVector a = if numOfVertexRows /= numOfColorRows
    then error "Number of vertex rows should be same as colors"
    else foldr
      (\i vec -> let vertexSlice = V.slice (i*2) 2 (vertices a); colorSlice = V.slice (i*3) 3 (colors a) in
        V.concat [vertexSlice,colorSlice,vec] )
      V.empty
      [0..(numOfVertexRows - 1)]
  where
    numOfVertexRows = V.length (vertices a) `div` 2
    numOfColorRows = V.length (colors a) `div` 3

mainIO :: IO ()
mainIO = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext window
  prog <- initResources


  ioRefVertices <- newIORef
    (V.fromList [  0.0,  0.5 , 1.0, 0.0, 0.0 -- Red
                , -0.5, -0.5 , 0.0, 1.0, 0.0 -- Green
                ,  0.5, -0.5 , 0.0, 0.0, 1.0 -- Blue
                ]
    ) :: IO(IORef(V.Vector Float))

  ioRefVertices2 <- newIORef VertexArrayObject {
    vertices = V.fromList
    [ 0.0, 0.5,
      -0.5, -0.5,
      0.5, -0.5 ],
    colors = V.fromList
      [1.0, 0.0, 0.0,
       0.0, 1.0, 0.0,
       0.0, 0.0, 1.0]
  }


  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        mapKeyboardState <- SDL.getKeyboardState

        let isKeyPressed key = mapKeyboardState SDL.Scancode {SDL.unwrapScancode = key}

        let isDownPressed = isKeyPressed SDL.Scancode.SDL_SCANCODE_DOWN
        let isUpPressed = isKeyPressed SDL.Scancode.SDL_SCANCODE_UP
        let isLeftPressed = isKeyPressed SDL.Scancode.SDL_SCANCODE_LEFT
        let isRightPressed = isKeyPressed SDL.Scancode.SDL_SCANCODE_RIGHT

        -- let modifyVertex (modifierX, modifierY) =
        --       modifyIORef ioRefVertices (\v -> let (fst:snd:rest) = V.toList v in V.fromList (fst `modifierX` 0.01: snd `modifierY` 0.01 :rest))

        -- when isDownPressed $ modifyVertex (const, (-))
        -- when isUpPressed $ modifyVertex (const, (+))
        -- when isLeftPressed $ modifyVertex ((-), const )
        -- when isRightPressed $ modifyVertex ((+),const)

        vertices <- readIORef ioRefVertices2

        when isDownPressed $ print $ vaoToVector vertices
        when isDownPressed $ print $ V.length $ vaoToVector vertices

        GL.clearColor $= GL.Color4 1 1 1 1
        GL.clear [GL.ColorBuffer]
        GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))
        location0 <- GL.uniformLocation prog "u_viewPort"
        GL.uniform location0 $= GL.Vector2 (realToFrac screenWidth :: GL.GLfloat) (realToFrac screenWidth :: GL.GLfloat)

        GL.attribLocation prog "position" $= GL.AttribLocation 0
        GL.attribLocation prog "vertex_color" $= GL.AttribLocation 1

        draw prog (GL.AttribLocation 0, GL.AttribLocation 1) $ vaoToVector vertices
        -- draw prog attrib vertices2
        SDL.glSwapWindow window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit


initResources :: IO GL.Program
initResources = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        er <- GL.get $ shaderInfoLog vs
        mapM_ (putStrLn . show) $ Data.Text.split (== '\n') $ Data.Text.pack er
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        er <- GL.get $ shaderInfoLog fs
        mapM_ (putStrLn . show) $ Data.Text.split (== '\n') $ Data.Text.pack er
        exitFailure

    program <- GL.createProgram

    GL.attachShader program vs
    GL.attachShader program fs

    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        exitFailure
    GL.currentProgram $= Just program

    return program

draw :: GL.Program -> (GL.AttribLocation, GL.AttribLocation) -> V.Vector Float -> IO ()
draw program (vertexAttribLocation, colorAttribLocation) vertices = do

    GL.currentProgram $= Just program
    GL.vertexAttribArray vertexAttribLocation $= GL.Enabled
    GL.vertexAttribArray colorAttribLocation $= GL.Enabled

    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer vertexAttribLocation $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ 5 * (sizeOf (0.0 :: Float))) ptr)

    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer colorAttribLocation $=
          (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 5 * (sizeOf (0.0 :: Float))) (plusPtr ptr (fromIntegral $ 2 * (sizeOf (0.0 :: Float))) ))

    GL.drawArrays GL.Triangles 0 3 -- 3 is the number of vertices

    GL.vertexAttribArray colorAttribLocation $= GL.Disabled
    GL.vertexAttribArray vertexAttribLocation $= GL.Disabled

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
             "#version 330 core"
           , "in vec2 position;"
           , "in vec3 vertex_color;"
           , "out vec3 fragment_color;"
           , ""
           , "void main() { "
           , " fragment_color = vertex_color;"
           , " gl_Position = vec4(position, 1.0, 1.0); "
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
             "#version 330 core"
           , "precision mediump float;"
           , ""
           , "uniform vec2 u_viewPort;"
           , "in vec3 fragment_color;"
           , ""
           , "void main() {"
           , "  vec2 normalizedFragCoord = gl_FragCoord.xy/u_viewPort;"
           , "  gl_FragColor = vec4(fragment_color, 1.0);"
           , "}"
           ]
