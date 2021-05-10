
"Okay, so because of how complex the collada file format is, I'll make a package after some time.
The idea is that I'll be able to easily define xml tags and have specialized behavior for each tag.
As an aside: I'll be creating my own file format (or research existing ones) for the 3d engine, to 
facilitate faster loading times, since, you know, you won't be transferring the game scenes between 
engines. If things heat up a bit, I'll develop an exporter for collada and alembic. Also, I'll eventually
add in an importer for the alembic file format (developed by Lucas Studios)"

(defun load-collada (file-name)
  (with-open-file (ip file-name)
    (let ((cur-line (read-line ip-stream nil)))
      (if cur-line
	  (if (not (string-equal cur-line "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
	      (return "No xml tag."))
	  (return "Improper format.")))
    ;;For Now, I'll support collada version 1.4.1, because that's what my 3d tool, blender, supports.
    ;;Eventually, I'll add in 1.5 support
    (loop for current-line = (read-line ip-stream nil)
       do (if (not current-line)
	      (return t))
	 
	 ))
  )
