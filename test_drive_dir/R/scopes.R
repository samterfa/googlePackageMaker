
	#' Get a list of scopes needed for an API function
	#' This function returns the list of needed scopes for a given API function.
	#'' @seealso \href{https://developers.google.com/identity/protocols/oauth2/scopes}
	#' @param function_name The name of the API function to return scopes for.
	#' @param return_all Whether to return all scopes for the API. Defaults to FALSE.
	#' @export
	get_function_scopes <- function(function_name = NULL, return_all = F){

		# Return all scopes
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly',
			'https://www.googleapis.com/auth/drive.scripts')

		if(return_all) return(scopes)


		# Return scopes for drive.about.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.about.get') return(scopes)


		# Return scopes for drive.changes.getStartPageToken
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.changes.getStartPageToken') return(scopes)


		# Return scopes for drive.changes.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.changes.list') return(scopes)


		# Return scopes for drive.changes.watch
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.changes.watch') return(scopes)


		# Return scopes for drive.channels.stop
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.channels.stop') return(scopes)


		# Return scopes for drive.comments.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.comments.create') return(scopes)


		# Return scopes for drive.comments.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.comments.delete') return(scopes)


		# Return scopes for drive.comments.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.comments.get') return(scopes)


		# Return scopes for drive.comments.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.comments.list') return(scopes)


		# Return scopes for drive.comments.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.comments.update') return(scopes)


		# Return scopes for drive.drives.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.drives.create') return(scopes)


		# Return scopes for drive.drives.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.drives.delete') return(scopes)


		# Return scopes for drive.drives.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.drives.get') return(scopes)


		# Return scopes for drive.drives.hide
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.drives.hide') return(scopes)


		# Return scopes for drive.drives.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.drives.list') return(scopes)


		# Return scopes for drive.drives.unhide
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.drives.unhide') return(scopes)


		# Return scopes for drive.drives.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.drives.update') return(scopes)


		# Return scopes for drive.files.copy
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.photos.readonly')

		if(function_name == 'drive.files.copy') return(scopes)


		# Return scopes for drive.files.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.files.create') return(scopes)


		# Return scopes for drive.files.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.files.delete') return(scopes)


		# Return scopes for drive.files.emptyTrash
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.files.emptyTrash') return(scopes)


		# Return scopes for drive.files.export
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.files.export') return(scopes)


		# Return scopes for drive.files.generateIds
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.files.generateIds') return(scopes)


		# Return scopes for drive.files.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.files.get') return(scopes)


		# Return scopes for drive.files.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.files.list') return(scopes)


		# Return scopes for drive.files.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.scripts')

		if(function_name == 'drive.files.update') return(scopes)


		# Return scopes for drive.files.watch
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.files.watch') return(scopes)


		# Return scopes for drive.permissions.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.permissions.create') return(scopes)


		# Return scopes for drive.permissions.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.permissions.delete') return(scopes)


		# Return scopes for drive.permissions.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.permissions.get') return(scopes)


		# Return scopes for drive.permissions.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.permissions.list') return(scopes)


		# Return scopes for drive.permissions.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.permissions.update') return(scopes)


		# Return scopes for drive.replies.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.replies.create') return(scopes)


		# Return scopes for drive.replies.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.replies.delete') return(scopes)


		# Return scopes for drive.replies.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.replies.get') return(scopes)


		# Return scopes for drive.replies.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.replies.list') return(scopes)


		# Return scopes for drive.replies.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.replies.update') return(scopes)


		# Return scopes for drive.revisions.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.revisions.delete') return(scopes)


		# Return scopes for drive.revisions.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.revisions.get') return(scopes)


		# Return scopes for drive.revisions.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file',
			'https://www.googleapis.com/auth/drive.metadata',
			'https://www.googleapis.com/auth/drive.metadata.readonly',
			'https://www.googleapis.com/auth/drive.photos.readonly',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.revisions.list') return(scopes)


		# Return scopes for drive.revisions.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.appdata',
			'https://www.googleapis.com/auth/drive.file')

		if(function_name == 'drive.revisions.update') return(scopes)


		# Return scopes for drive.teamdrives.create
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.teamdrives.create') return(scopes)


		# Return scopes for drive.teamdrives.delete
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.teamdrives.delete') return(scopes)


		# Return scopes for drive.teamdrives.get
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.teamdrives.get') return(scopes)


		# Return scopes for drive.teamdrives.list
		scopes <- c(
			'https://www.googleapis.com/auth/drive',
			'https://www.googleapis.com/auth/drive.readonly')

		if(function_name == 'drive.teamdrives.list') return(scopes)


		# Return scopes for drive.teamdrives.update
		scopes <- c(
			'https://www.googleapis.com/auth/drive')

		if(function_name == 'drive.teamdrives.update') return(scopes)
	}
