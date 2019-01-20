(*
  auteur : FIL
  date   : janvier 2010
  objet  : lecture et sauvegarde de fichiers image dans différents format 
   (PGM, PPM, JPG, PNG, GIF, ...)
  necessite l'installation de la commande convert de la suite de traitements
  d'images Image Magick (http://www.imagemagick.org/)
*)

module Image_magick :
  sig
		(**
			 Module Images

			 Permet de charger et sauvegarder des images stockées dans des fichiers dans divers formats : png, jpg, gif, bmp, pgm, ppm.

			 Necessite la suite logicielle ImageMagick.



			 L'utilisation de ce module en mode interprete necessite l'appel à l'interpreteur avec les options :

			 - [ocaml graphics.cma images.cmo]


			 La production d'un executable utilisant ce module doit se faire avec la commande :

			 - [ocamlc -o <nom_executable>  graphics.cma images.cmo  <source_a_compiler>]

			 @author FIL - IEEA - Univ. Lille1 (mars 2010)

			 @see <http://www.imagemagick.org/> le site d'ImageMagick.
		*)

		(** {2 Lecture et ecriture d'images dans des fichiers} *)

		(**
			 [liste_formats] = liste des formats autorises.
		*)
		val liste_formats : string list


		(**
			 [lire_image s] = tableau de pixels represente par leur couleur, correspondant a l'image stockee dans le fichier nomme [s].

			 {b CU :} le fichier nomme [s] doit exister et l'extension de son nom doit correspondre a l'un des formats autorises .
		*)
		val lire_image : string -> Graphics.color array array

		(**
			 [sauver_image t s] sauvegarde l'image representee par le tableau de pixels [t] dans un fichier nomme [s]. Le format de sauvegarde est determine par l'extension choisie dans le nom [s]. 

			 {b CU :} le format doit etre l'un des formats autorises.
		*)
		val sauver_image : Graphics.color array array -> string -> unit

		(**  {2 Dessiner une image dans une fenetre graphique} *)

		(**
			 [dessiner_image t] dessine dans la fenetre graphique l'image representee par le tableau de pixels [t]. Le dessin est fait dans le coin inferieur gauche de la fenetre graphique.

			 {b CU :} une fenetre graphique doit etre prealablement ouverte.
		*)
		val dessiner_image : Graphics.color array array -> unit
  end =

  struct

		let suffixe_tmp = ".tmp "
		and rm = 
			if Sys.os_type="Unix" then
				"rm -f "
			else
				"del " 
		and mv = 
			if Sys.os_type="Unix" then
				"mv "
			else
				"move " 
		and dev_null =
			if Sys.os_type="Unix" then
				" 2> /dev/null"
			else
				"" ;;


		open Graphics 

		(*
			fonction
				lire_image_ppm : string -> color array array
			parametre 
				nom : string = nom du fichier pgm ou ppm 
		*  valeur renvoyee : color array array = tableau des pixels
			CU : 
				- suppose qu'un fichier nommé nom contenant une image au format pgm existe 
				- n'accepte pas de ligne de commentaire (commencant par #) dans l'entete du fichier
		*)
		let lire_image_ppm nom = 
			let entree = open_in_bin nom 
			in
				let format = input_line entree 
				and largeur,hauteur = 
				  let ligne = ref (input_line entree)
				  in
			while !ligne.[0] = '#' do
				ligne := input_line entree
			done ;
			Scanf.sscanf 
				!ligne 
				      "%d %d" 
				(fun x y -> x,y)
				and _ = input_line entree (* lecture de la ligne contenant 255 *)
				in
				  let img = Array.make_matrix hauteur largeur (rgb 0 0 0)
				  and en_couleur = (format = "P6")
				  in
			for i = 0 to hauteur - 1 do
				for j = 0 to largeur - 1 do
					img.(i).(j) <- 
					  if en_couleur then
				let x = input_byte entree
				and y = input_byte entree
				and z = input_byte entree
				in 
					rgb x y z
					  else
				let x = input_byte entree 
				in 
					rgb x x x
				done
			done ;
			close_in entree ;
			img ;;


		(*
			fonction lire_image : string -> color array array
			parametre 
				nom : string = nom du fichier image a lire
			valeur renvoyee : color array array = tableau des pixels de 
				       l'image contenue dans le fichier
			CU : nom doit etre un fichier image d'un format courant 
			 (ie connu de l'utilitaire convert de la suite ImageMagick)
			
		*)
		let lire_image nom =
			let r = Sys.command ("convert -depth 8 "^nom^" "^nom^".ppm "^dev_null)
			in
				if r <> 0 then
				  failwith ("lire_image : fichier "^nom^" manquant ou pas dans un format image")
				else
				  let res = lire_image_ppm (nom^".ppm")
				  in
			ignore(Sys.command (rm^nom^".ppm"));
			res;;

				
		(*
			procedure 
				dessiner_image : color array array -> unit
			parametre 
				img : color array array = image a dessiner
			action : dessine l'image dans le coin inferieur gauche
			CU : une fenetre graphique doit prealablement etre ouverte
		*)
		let dessiner_image img =
			draw_image (make_image img) 0 0;;


		(*
			procedure
				sauver_image_pgm : color array array -> string -> unit
			parametres
				img : color array array = image a sauvegarder
				nom : string = nom du fichier de sauvegarde de l'image
			action : sauvegarde de l'image au format PPM, 
		*)
		let sauver_image_ppm (img : Graphics.color array array) nom = 
			let sortie = open_out_bin nom
			and hauteur = Array.length img
			and largeur = Array.length img.(0)
			in
				output_string sortie "P6\n" ;
				output_string sortie ((string_of_int largeur)^" "^(string_of_int hauteur)^"\n") ;
				output_string sortie "255\n";
				for i = 0 to hauteur - 1 do
				  for j = 0 to largeur - 1 do
			let r = img.(i).(j) / (256*256)
			and g = (img.(i).(j) mod (256*256)) / 256
			and b = img.(i).(j) mod 256
			in
				output_byte sortie r ;
				output_byte sortie g ;
				output_byte sortie b
				  done
				done ;
				close_out sortie;;


		let liste_formats = [".png"; ".jpg"; ".gif"; ".bmp"; ".pgm"; ".ppm"] ;;
				
		(*
			procedure
				sauver_image : color array array -> string -> unit
			parametres
				img : color array array = image a sauvegarder
				nom : string = nom du fichier de sauvegarde de l'image
			action : sauvegarde de l'image dans un fichier nomme par nom 
			CU : le nom du fichier doit se terminer par une extension 
				   indiquant le format qui doit faire partie de la liste 
				   liste_formats
		*)
		let sauver_image img nom = 
			let suffixe = String.sub nom ((String.length nom) - 4) 4
			in
				if not (List.mem suffixe liste_formats) then
				  failwith "sauver_image : format image non reconnu"
				else
				  let _ = sauver_image_ppm img (nom^".tmp")
				  in
			if suffixe="ppm" then
				ignore(Sys.command (mv^nom^suffixe_tmp^nom))
			else begin
				ignore(Sys.command ("convert "^nom^suffixe_tmp^" "^nom^dev_null)) ;
				ignore(Sys.command (rm^nom^suffixe_tmp))
			end;;

	end
