#|

A file for some "stock" shaders

|#
(push "So, I'm not sure if cl-opengl has support for glsl 3.3 and above, or if it's my gfx card. So, because of current limitations, all of the shaders will be 1.3 core or 3.2 es compliant. See:     https://www.khronos.org/registry/OpenGL/specs/es/3.2/GLSL_ES_Specification_3.20.html   and   https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.1.30.pdf   for information on glsl 3.2 es and 1.3 core." tamias-3d-messages)
(push "Using Shaders: (create-shader \"Shader name\" :shader-type \"Shader text\")
Currently, my convention for the name is...uh...underscore style? \"Sun_lighting_stock_shader_v_1_0\"
The Three shader types currently supported are :vertex :fragment and :geometry... nevermind about :geometry, it is not supported in glsl 1.3
The shader text is the actual body of the shader program, i.e the vec4, mat4, in, out, {...} stuff
Every time a shader is created, it increases the current-id of that shader's table
(get-highest-id :vertex) will get the 'current id' of the vertex shader table
I may end up either creating an auxilary table which will have symbols as keys, or variables will be declared that hold their ids (could even do a constant)" tamias-3d-messages)
(push "See: from: https://veeenu.github.io/2014/04/22/implementing-keyframe-animation.html and https://veeenu.github.io/2014/05/09/implementing-skeletal-animation.html for some information on animation." tamias-3d-messages)
(defstruct tamias-shader
  name
  id
  string
  error)
(defvar tamias-vertex-shaders (make-hashtable))
(defvar tamias-fragment-shaders (make-hashtable))
(defvar tamias-geometry-shaders (make-hashtable))
(defvar current-vertex-shader-id 0)
(defvar current-fragment-shader-id 0)
(defvar current-geometry-shader-id 0)

"On compilation of shader, run (gl:get-shader-info-log SHADER) and put it into tamias-shader-error slot"
    
(defmacro make-shader (shader-id shader-table shader-name shader-string)
  `(setf (gethash ,shader-id ,shader-table) (make-tamias-shader
					     :name ,shader-name
					     :id ,shader-id
					     :string ,shader-string)))

(defun get-shader (shader-id shader-type)
  (case shader-type
    (:vertex (gethash shader-id tamias-vertex-shaders))
    (:fragment (gethash shader-id tamias-fragment-shaders))
    (:geometry (gethash shader-id tamias-geometry-shaders))))

(defun get-shader-id (shader-symbol shader-type)
  )

(defun get-shader-string (shader-id shader-type)
  (tamias-shader-string (get-shader shader-id shader-type)))

(defun get-highest-id (shader-type)
  (case shader-type
    (:vertex current-vertex-shader-id)
    (:fragment current-fragment-shader-id)
    (:geometry current-geometry-shader-id)))

(defun create-shader (shader-name shader-type shader-string)
  (case shader-type
    (:vertex (make-shader current-vertex-shader-id tamias-vertex-shaders
			  shader-name shader-string))
    (:fragment (make-shader current-fragment-shader-id tamias-fragment-shaders
			  shader-name shader-string))
    (:geometry (make-shader current-geometry-shader-id tamias-geometry-shaders
			  shader-name shader-string))
    ))

#|
(gl:Bind-Attrib-Location
|#
"For the purposes of the engine, an 'animation' is anything that actively deforms the vertices of an object/model. A simple roation or translation is not an 'animation'."

(create-shader "Bone_animation_basic" :vertex
"#version 320 es
//Taken from: https://www.khronos.org/opengl/wiki/Skeletal_Animation
//
// Vertex Shader
attribute vec4 Vertex;
attribute vec3 Normal;
attribute vec2 TexCoord;
attribute vec2 Index; //2 bones
attribute vec2 Weight; //2 bone weights, respective of bone indices
uniform mat4 ModelviewMatrix;
uniform mat4 ProjectionModelviewMatrix;
uniform mat4 Bone[10]; // Array of bones that you compute (animate) on the CPU and you upload to the shader
// --------------------
varying vec2 TexCoord0;
varying vec3 EyeNormal;
// --------------------
void main()
{
    vec4 newVertex;
    vec4 newNormal;
    int index;
    // --------------------
    index=int(Index.x); // Cast to int
    newVertex = (Bone[index] * Vertex) * Weight.x;
    newNormal = (Bone[index] * vec4(Normal, 0.0)) * Weight.x;
    index=int(Index.y); //Cast to int
    newVertex = (Bone[index] * Vertex) * Weight.y + newVertex;
    newNormal = (Bone[index] * vec4(Normal, 0.0)) * Weight.y + newNormal;
    EyeNormal = vec3(ModelviewMatrix * newNormal);
    gl_Position = ProjectionModelviewMatrix * newVertex;
    TexCoord0 = TexCoord;
}")
(create-shader "Texture_basic_v" :vertex
"#version 320 es

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec2 vertexUV;

// Output data ; will be interpolated for each fragment.
out vec2 UV;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;

void main(){

    // Output position of the vertex, in clip space : MVP * position
    gl_Position =  MVP * vec4(vertexPosition_modelspace,1);

    // UV of the vertex. No special space for this one.
    UV = vertexUV;
}")

(create-shader "Texture_basic_f" :fragment
"#version 320 es
//https://www.opengl-tutorial.org/beginners-tutorials/tutorial-5-a-textured-cube/
// Interpolated values from the vertex shaders
in vec2 UV;

// Ouput data
out vec3 color;

// Values that stay constant for the whole mesh.
uniform sampler2D myTextureSampler;

void main(){

    // Output color = color of the texture at the specified UV
    color = texture( myTextureSampler, UV ).rgb;
}")

(create-shader "KF_animation_basic_v" :vertex
"#version 320 es
//from: https://veeenu.github.io/2014/04/22/implementing-keyframe-animation.html
uniform mat4 uP, uV, uM;
uniform mat3 uN;

attribute vec3 aVertex, aNormal;
attribute vec2 aTexCoord;

varying vec3 vVertex, vNormal;

void main() {

  gl_Position = uP * uV * uM * vec4(aVertex, 1.0);
  vVertex = aVertex;
  vNormal = aNormal;

}")
(create-shader "KF_animation_basic_f" :fragment
"#version 320 es
//from: https://veeenu.github.io/2014/04/22/implementing-keyframe-animation.html
precision highp float;

uniform mat4 uP, uV, uM;
uniform mat3 uN;

uniform sampler2D uSampler;
varying vec3 vVertex, vNormal;

void main() {

  vec3 lAmbient = vec3(1.0, 1.0, 1.0);
  vec3 lDiffuse = vec3(0.5, 0.5, 1.0);
  vec3 lSpecular= vec3(1.0, 1.0, 1.0);

  vec3 plPos = vec3(0.0, 3.0, 5.0);
  vec3 plDir = normalize(plPos - vVertex);

  mat4 mvp = uP * uV * uM;
  vec3 n = normalize(uN * vNormal);
  vec3 l = normalize(vec3(mvp * vec4(plDir, 1.0)));
  vec3 v = normalize(-vec3(mvp * vec4(vVertex, 1.0)));
  vec3 r = reflect(l, n);

  float lambert = dot(l, n),
        ambientInt = 0.2,
        specularInt = 0.5,
        diffuseInt = 1.0,
        shininess = 128.0;

  float specular = pow( max( 0.0, dot(r,v) ), shininess );

  gl_FragColor = vec4(
      lAmbient * ambientInt +
      lDiffuse * diffuseInt * lambert +
      lSpecular * specularInt * specular
      , 1.0);
}")
