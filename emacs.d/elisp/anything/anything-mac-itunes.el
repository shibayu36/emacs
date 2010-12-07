;;; anything-mac-itunes.el --- Use iTunes on Mac with anything.el

;; Copyright (C) 2010  Hiroshige Umino

;; Author: Hiroshige Umino <yaotti@gmail.com>
;; Created: 2010-11-24
;; Version: 0.0.1
;;
;; Keywords: anything, mac
;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; Commentary:
;;
;;
;; You just require anything-mac-itunes like:
;;
;; (require 'anything-mac-itunes)
;;
;; then execute M-x anything-mac-itunes to choose and play track
;; with anything interface.
;; You can search tracks by track title, artist name and album title.
;;
;;
;; These methods are also useful. Try these with M-x ...
;; `anything-mac-itunes-show-current-track-info'
;; `anything-mac-itunes-playpause-track'
;; `anything-mac-itunes-next-track'
;; `anything-mac-itunes-back-track'
;; `anything-mac-itunes-previous-track'
;;


(require 'anything)

;; "最近再生した項目" in Japanese or "Recently Played" in English.
(defvar anything-mac-itunes-recently-played-playlist
  "最近再生した項目"
  "Playlist name which contains recently played tracks in iTunes")

;; "ライブラリ" in Japanese or "Library" in English.
(defvar anything-mac-itunes-library-name
  "ライブラリ"
  "Library name in iTunes. \"Library\" in English or \"ライブラリ\" in Japanese.")

;; Applescript to get all songs in specific playlist
(defvar anything-mac-itunes-script-for-playlist
  "set formattedTrackTitles to {}
tell application \"iTunes\"
	set allTracks to (every file track of playlist \"%s\")
	repeat with aTrack in allTracks
		set aTrackTitle to name of aTrack
		set artistName to artist of aTrack
		set albumTitle to album of aTrack
		if artistName is \"\" then
			set artistName to \"Unknown Artist\"
		end if
		if albumTitle is \"\" then
			set albumTitle to \"Unknown Album\"
		end if
		set end of formattedTrackTitles to aTrackTitle & \" / \" & artistName & \" (\" & albumTitle & \")\"
	end repeat
end tell
set text item delimiters of AppleScript to \"\n\"
return formattedTrackTitles as text")

;; An anything action to play some song. The format of
;; argument is like : "TRACKNAME / ARTISTNAME (ALBUMNAME)"
(defun anything-mac-itunes-action--play (candidate)
  (let ((name-and-artist (split-string candidate " / \\| (\\|)")))
    (multiple-value-bind
        (name artist album) name-and-artist
      (anything-mac-itunes-play-track name artist album))))

;; Anything source for iTunes musics.
(defvar anything-c-source-mac-itunes-tracks
  '((name . "All tracks")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (with-current-buffer
                    (anything-candidate-buffer 'global)
                  (message "Building all tracks' information list...")
                  (insert (do-applescript
                           (format anything-mac-itunes-script-for-playlist anything-mac-itunes-library-name)))))))
    (candidates-in-buffer)
    (action . (("Play" . anything-mac-itunes-action--play)))))
;; (anything anything-c-source-mac-itunes-tracks)

(defvar anything-c-source-mac-itunes-recently-played-tracks
  '((name . "Recently played tracks")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (with-current-buffer
                    (anything-candidate-buffer 'global)
                  (insert (do-applescript
                           (format anything-mac-itunes-script-for-playlist
                                   anything-mac-itunes-recently-played-playlist)))))))
    (candidates-in-buffer)
    (action . (("Play" . anything-mac-itunes-action--play)))))

;; (anything anything-c-source-mac-itunes-recently-played-tracks)

(defun anything-mac-itunes-do-applescript-for-itunes (body)
  (do-applescript
   (format "tell application \"iTunes\"
	%s
end tell" body)))

(defun anything-mac-itunes-play-track (name artist album)
  (let ((body
         (format "play track \"%s\" -- of artist \"%s\""
                 name artist)))
    (anything-mac-itunes-do-applescript-for-itunes body)
    (message (format "Playing: %s / %s (%s)" name artist album))))


(defun anything-mac-itunes-show-current-track-info ()
  "Show current track's info in minibuffer"
  (interactive)
  (let* ((delimiter "\n")
         (body (format
                "name of current track & \"%s\" & artist of current track & \"%s\" & current track's album" delimiter delimiter))
         (info (split-string
                (anything-mac-itunes-do-applescript-for-itunes body)
                delimiter))
         (name (car info))
         (artist (cadr info))
         (album (caddr info)))
    (message (format "%s / %s (%s)" name artist album))))

(defun anything-mac-itunes-playpause-track ()
  "Play/Pause the current track"
  (interactive)
  (anything-mac-itunes-do-applescript-for-itunes "playpause")
  (anything-mac-itunes-show-current-track-info))

(defun anything-mac-itunes-next-track ()
  "Play the next track"
  (interactive)
  (anything-mac-itunes-do-applescript-for-itunes "next track")
  (anything-mac-itunes-show-current-track-info))

(defun anything-mac-itunes-back-track ()
  "Playback the track"
  (interactive)
  (anything-mac-itunes-do-applescript-for-itunes "back track")
  (anything-mac-itunes-show-current-track-info))

(defun anything-mac-itunes-previous-track ()
  "Play the previous track"
  (interactive)
  (anything-mac-itunes-do-applescript-for-itunes "previous track")
  (anything-mac-itunes-show-current-track-info))

(defun anything-mac-itunes ()
  "Select iTunes music with anything and play it"
  (interactive)
  (anything-other-buffer
   '(anything-c-source-mac-itunes-recently-played-tracks
     anything-c-source-mac-itunes-tracks)
   "*Anything Mac iTunes*"))

(provide 'anything-mac-itunes)
