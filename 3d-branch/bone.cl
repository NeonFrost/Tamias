#|

A preliminary examination of Bone animations in 3d engines.
Or:
An idiot tries to understand a matrix of partial differentials

Some of this information is going to be incorrect, because I am an idiot. If something sounds off, then look it up (like lattices)


So, essentially, a bone, joint, rig, or otherwise "Armature" affects where the vertices of a mesh/object appear to be.
A bone has: rotation -"Global" - spins on global axis
                     -Local - spins on local axis
            position -Global
                     -Local
            scale
            Vertices affected
            weights

A bone that is connected at the "elbow" and "wrist" will affect the vertices near the "elbow" differently than the "wrist"
Example: Stretch your arm out. Now Flex your elbow so that your bicep is, roughly, parallel with the floor, while your forearm is perpendicular ( \___| | )
(                                                                                                                                                 ______| )

The two ways to do animation are very very simple:
You have an armature set up, have keyframes set up, interpolate between them, then send it through the render pipeline. This is a cpu problem
Or
You "bake" or create a "cache" of the animation for every frame the animation plays, describing the positions of each vertex. This is a memory problem

Currently, I plan on doing both. Baking for "static", i.e. non-destroyable :: not affected by world physics, "entities" (think: A non-destroyable bridge)
...                              Armatures for "dynamic", i.e. humanoids :: objects affected by world physics, "entities" (think: A bat)
...                              Of course, this creates a problem with stuff that's affected by world physics that are neither static nor dynamic, like cloth that isn't destroyable
...                                                     Note: this last thing is solved with, at least I believe it's solved by, a thing called a Lattice
...                                                            think: That grid with a planet that demonstrates Einstein's idea (general relativity?)


Essentially you have things like "VAOs" set up that contain all of the vertex information, send it through an "animation" shader, then do other shader stuff.
So
Shaders:
Animation
Texture
Bump
Normals
Lighting
Other post-processing


So, this will be a temporary area to display how to do VAO and VBO things


FROM: https://learnopengl.com/Model-Loading/Mesh
void setupMesh()
{
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &EBO);
  
    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);

    glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(Vertex), &vertices[0], GL_STATIC_DRAW);  

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.size() * sizeof(unsigned int), 
                 &indices[0], GL_STATIC_DRAW);

    // vertex positions
    glEnableVertexAttribArray(0);	
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);
    // vertex normals
    glEnableVertexAttribArray(1);	
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, Normal));
    // vertex texture coords
    glEnableVertexAttribArray(2);	
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, TexCoords));

    glBindVertexArray(0);
}  
The Equivalent in CL:
(defun setup-mesh ()
  (let ((buffers (gl:gen-buffers 2)))
    (setf vertex-buffer (elt buffers 0)
	  index-buffer (elt buffers 1)))
  (gl:bind-buffer :array-buffer (vertex-buffer w))
  (let ((arr (gl:alloc-gl-array :float 12))
	(verts #(-0.5 -0.5 0.0 
		 -0.5 0.5 0.0 
		 0.5 -0.5 0.0 
		 0.5 0.5 0.0)))
    (dotimes (i (length verts))
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :array-buffer 0)

  (gl:bind-buffer :element-array-buffer (index-buffer w))
  (let ((arr (gl:alloc-gl-array :unsigned-short 6))
	(indexes #(0 2 1 1 2 3)))
    (dotimes (i (length indexes))
      (setf (gl:glaref arr i) (aref indexes i)))
    (gl:buffer-data :element-array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (gl:bind-buffer :element-array-buffer 0)
  (setf (vertex-array w) (gl:gen-vertex-array))
  (gl:bind-vertex-array (vertex-array w))

    // vertex positions
    glEnableVertexAttribArray(0);	
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);
    // vertex normals
    glEnableVertexAttribArray(1);	
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, Normal));
    // vertex texture coords
    glEnableVertexAttribArray(2);	
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, TexCoords));

    glBindVertexArray(0);
)


    |#
(gl:vertex-attrib-pointer )
"The rig subsystem is an ID system
When a rig is created, it is given an id
When a rig is needed, the model-rig-id is used to fetch a rig from the rig id table
     Then, it is copied into the model (model-rig ...)
A rig is composed of bones, which communicate with the shader"

(define-id-system tamias-animations)

(defstruct tamias-animation
  name
  id
  keyframes;;an array of keyframes
  rig)

(defstruct rig
  bones;;an array of transformed-bones
  bind-bones
  id
  weights);;structure: [v_ind b_ind weight]
(defstruct bone
  (number 0)
  parent
  children
  (head (make-array '(3) :initial-contents '(0.0 0.0 0.0))) ;;if not a parent, should be where the paren'ts tail is
  (tail (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
  
  (scale (make-array '(3) :initial-contents '(1.0 1.0 1.0)))
  (rotation (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
  (position (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
  (roll 0.0))   ;;this is for when the head and tail positions don't change (a rotation along the x, y, or z axes can be represented as a change in either the head, tail or both positions)
;;  weights ;;structured like so: [V_index . V_weight,... Vn_index . Vn_weight];;THIS HAS BEEN MOVED INTO THE MODEL
(defstruct (bind-bone (:include bone))
  (bind-matrix (make-array '(4 4) :initial-contents '((1.0 0.0 0.0 0.0) ;;default position of the bone with no transformations applied
						      (0.0 1.0 0.0 0.0) ;;or: these are the transformations that result in the default pose (think: t-pose)
						      (0.0 0.0 1.0 0.0)
						      (0.0 0.0 0.0 1.0))))
  (inverse-bind-matrix (make-mat4))
  )
(defstruct (posed-bone (:include bone))
  (matrix (make-mat4))
  )
(defstruct (transformed-bone (:include posed-bone)))

(defstruct keyframe
  (number 0)
  bones
#|  (rotation (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
  (position (make-array '(3) :initial-contents '(0.0 0.0 0.0)))
  (scale (make-array '(3) :initial-contents '(1.0 1.0 1.0)))
  (roll 0.0)|#
  )
"I'm not terribly sure how to do everything related to animation, especially bone animation (jacobian matrices and what not)
So, things are subject to change. Like I don't know if I should have a list of frames like (frame-number '(bone-num head-x tail-x head-y tail-y head-z tail-z roll) ...) or what"

"Alright, so, this is headache zone. Zing bop shizam! So, when an animation is loaded it will be created as an tamias animation. Once all of the data is collected from the file, it is added to the tamias-animations table.
Also, to start off, the engine will NOT handle bezier curves. Once I get linear interpolation up and running (even if inaccurate, due to my files) then I'll work on implementing bezier interpolation.
"
