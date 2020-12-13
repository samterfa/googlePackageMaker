
	#' Get a list of scopes needed for an API function
	#' This function returns the list of needed scopes for a given API function.
	#'' @seealso \href{https://developers.google.com/identity/protocols/oauth2/scopes}
	#' @param function_name The name of the API function to return scopes for.
	#' @param return_all Whether to return all scopes for the API. Defaults to FALSE.
	#' @export
	get_function_scopes <- function(function_name = NULL, return_all = F){

		# Return all scopes
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security',
			'https://www.googleapis.com/auth/cloud-platform',
			'https://www.googleapis.com/auth/admin.directory.device.chromeos',
			'https://www.googleapis.com/auth/admin.directory.userschema.readonly',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.readonly',
			'https://www.googleapis.com/auth/admin.directory.group.member.readonly',
			'https://www.googleapis.com/auth/admin.directory.orgunit',
			'https://www.googleapis.com/auth/admin.directory.user.readonly',
			'https://www.googleapis.com/auth/admin.directory.user.alias.readonly',
			'https://www.googleapis.com/auth/admin.directory.orgunit.readonly',
			'https://www.googleapis.com/auth/admin.directory.customer',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly',
			'https://www.googleapis.com/auth/admin.directory.group.readonly',
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.device.chromeos.readonly',
			'https://www.googleapis.com/auth/admin.directory.customer.readonly',
			'https://www.googleapis.com/auth/admin.directory.domain',
			'https://www.googleapis.com/auth/admin.directory.domain.readonly',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.group.member',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly',
			'https://www.googleapis.com/auth/admin.directory.user.alias',
			'https://www.googleapis.com/auth/admin.directory.userschema',
			'https://www.googleapis.com/auth/admin.directory.device.mobile',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.action')

		if(return_all) return(scopes)


		# Return scopes for directory.asps.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.asps.delete') return(scopes)


		# Return scopes for directory.asps.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.asps.get') return(scopes)


		# Return scopes for directory.asps.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.asps.list') return(scopes)


		# Return scopes for admin.channels.stop
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.alias',
			'https://www.googleapis.com/auth/admin.directory.user.alias.readonly',
			'https://www.googleapis.com/auth/admin.directory.user.readonly',
			'https://www.googleapis.com/auth/cloud-platform')

		if(function_name == 'admin.channels.stop') return(scopes)


		# Return scopes for directory.chromeosdevices.action
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos')

		if(function_name == 'directory.chromeosdevices.action') return(scopes)


		# Return scopes for directory.chromeosdevices.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos',
			'https://www.googleapis.com/auth/admin.directory.device.chromeos.readonly')

		if(function_name == 'directory.chromeosdevices.get') return(scopes)


		# Return scopes for directory.chromeosdevices.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos',
			'https://www.googleapis.com/auth/admin.directory.device.chromeos.readonly')

		if(function_name == 'directory.chromeosdevices.list') return(scopes)


		# Return scopes for directory.chromeosdevices.moveDevicesToOu
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos')

		if(function_name == 'directory.chromeosdevices.moveDevicesToOu') return(scopes)


		# Return scopes for directory.chromeosdevices.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos')

		if(function_name == 'directory.chromeosdevices.patch') return(scopes)


		# Return scopes for directory.chromeosdevices.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos')

		if(function_name == 'directory.chromeosdevices.update') return(scopes)


		# Return scopes for admin.customer.devices.chromeos.issueCommand
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos')

		if(function_name == 'admin.customer.devices.chromeos.issueCommand') return(scopes)


		# Return scopes for admin.customer.devices.chromeos.commands.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.chromeos',
			'https://www.googleapis.com/auth/admin.directory.device.chromeos.readonly')

		if(function_name == 'admin.customer.devices.chromeos.commands.get') return(scopes)


		# Return scopes for directory.customers.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.customer',
			'https://www.googleapis.com/auth/admin.directory.customer.readonly')

		if(function_name == 'directory.customers.get') return(scopes)


		# Return scopes for directory.customers.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.customer')

		if(function_name == 'directory.customers.patch') return(scopes)


		# Return scopes for directory.customers.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.customer')

		if(function_name == 'directory.customers.update') return(scopes)


		# Return scopes for directory.domainAliases.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain')

		if(function_name == 'directory.domainAliases.delete') return(scopes)


		# Return scopes for directory.domainAliases.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain',
			'https://www.googleapis.com/auth/admin.directory.domain.readonly')

		if(function_name == 'directory.domainAliases.get') return(scopes)


		# Return scopes for directory.domainAliases.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain')

		if(function_name == 'directory.domainAliases.insert') return(scopes)


		# Return scopes for directory.domainAliases.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain',
			'https://www.googleapis.com/auth/admin.directory.domain.readonly')

		if(function_name == 'directory.domainAliases.list') return(scopes)


		# Return scopes for directory.domains.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain')

		if(function_name == 'directory.domains.delete') return(scopes)


		# Return scopes for directory.domains.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain',
			'https://www.googleapis.com/auth/admin.directory.domain.readonly')

		if(function_name == 'directory.domains.get') return(scopes)


		# Return scopes for directory.domains.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain')

		if(function_name == 'directory.domains.insert') return(scopes)


		# Return scopes for directory.domains.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.domain',
			'https://www.googleapis.com/auth/admin.directory.domain.readonly')

		if(function_name == 'directory.domains.list') return(scopes)


		# Return scopes for directory.groups.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.delete') return(scopes)


		# Return scopes for directory.groups.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.groups.get') return(scopes)


		# Return scopes for directory.groups.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.insert') return(scopes)


		# Return scopes for directory.groups.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.groups.list') return(scopes)


		# Return scopes for directory.groups.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.patch') return(scopes)


		# Return scopes for directory.groups.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.update') return(scopes)


		# Return scopes for directory.groups.aliases.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.aliases.delete') return(scopes)


		# Return scopes for directory.groups.aliases.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group')

		if(function_name == 'directory.groups.aliases.insert') return(scopes)


		# Return scopes for directory.groups.aliases.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.groups.aliases.list') return(scopes)


		# Return scopes for directory.members.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member')

		if(function_name == 'directory.members.delete') return(scopes)


		# Return scopes for directory.members.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member',
			'https://www.googleapis.com/auth/admin.directory.group.member.readonly',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.members.get') return(scopes)


		# Return scopes for directory.members.hasMember
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member',
			'https://www.googleapis.com/auth/admin.directory.group.member.readonly',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.members.hasMember') return(scopes)


		# Return scopes for directory.members.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member')

		if(function_name == 'directory.members.insert') return(scopes)


		# Return scopes for directory.members.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member',
			'https://www.googleapis.com/auth/admin.directory.group.member.readonly',
			'https://www.googleapis.com/auth/admin.directory.group.readonly')

		if(function_name == 'directory.members.list') return(scopes)


		# Return scopes for directory.members.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member')

		if(function_name == 'directory.members.patch') return(scopes)


		# Return scopes for directory.members.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.group',
			'https://www.googleapis.com/auth/admin.directory.group.member')

		if(function_name == 'directory.members.update') return(scopes)


		# Return scopes for directory.mobiledevices.action
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.mobile',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.action')

		if(function_name == 'directory.mobiledevices.action') return(scopes)


		# Return scopes for directory.mobiledevices.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.mobile')

		if(function_name == 'directory.mobiledevices.delete') return(scopes)


		# Return scopes for directory.mobiledevices.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.mobile',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.action',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.readonly')

		if(function_name == 'directory.mobiledevices.get') return(scopes)


		# Return scopes for directory.mobiledevices.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.device.mobile',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.action',
			'https://www.googleapis.com/auth/admin.directory.device.mobile.readonly')

		if(function_name == 'directory.mobiledevices.list') return(scopes)


		# Return scopes for directory.orgunits.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit')

		if(function_name == 'directory.orgunits.delete') return(scopes)


		# Return scopes for directory.orgunits.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit',
			'https://www.googleapis.com/auth/admin.directory.orgunit.readonly')

		if(function_name == 'directory.orgunits.get') return(scopes)


		# Return scopes for directory.orgunits.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit')

		if(function_name == 'directory.orgunits.insert') return(scopes)


		# Return scopes for directory.orgunits.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit',
			'https://www.googleapis.com/auth/admin.directory.orgunit.readonly')

		if(function_name == 'directory.orgunits.list') return(scopes)


		# Return scopes for directory.orgunits.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit')

		if(function_name == 'directory.orgunits.patch') return(scopes)


		# Return scopes for directory.orgunits.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.orgunit')

		if(function_name == 'directory.orgunits.update') return(scopes)


		# Return scopes for directory.privileges.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly')

		if(function_name == 'directory.privileges.list') return(scopes)


		# Return scopes for directory.resources.buildings.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.buildings.delete') return(scopes)


		# Return scopes for directory.resources.buildings.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.buildings.get') return(scopes)


		# Return scopes for directory.resources.buildings.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.buildings.insert') return(scopes)


		# Return scopes for directory.resources.buildings.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.buildings.list') return(scopes)


		# Return scopes for directory.resources.buildings.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.buildings.patch') return(scopes)


		# Return scopes for directory.resources.buildings.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.buildings.update') return(scopes)


		# Return scopes for directory.resources.calendars.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.calendars.delete') return(scopes)


		# Return scopes for directory.resources.calendars.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.calendars.get') return(scopes)


		# Return scopes for directory.resources.calendars.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.calendars.insert') return(scopes)


		# Return scopes for directory.resources.calendars.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.calendars.list') return(scopes)


		# Return scopes for directory.resources.calendars.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.calendars.patch') return(scopes)


		# Return scopes for directory.resources.calendars.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.calendars.update') return(scopes)


		# Return scopes for directory.resources.features.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.features.delete') return(scopes)


		# Return scopes for directory.resources.features.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.features.get') return(scopes)


		# Return scopes for directory.resources.features.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.features.insert') return(scopes)


		# Return scopes for directory.resources.features.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar',
			'https://www.googleapis.com/auth/admin.directory.resource.calendar.readonly')

		if(function_name == 'directory.resources.features.list') return(scopes)


		# Return scopes for directory.resources.features.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.features.patch') return(scopes)


		# Return scopes for directory.resources.features.rename
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.features.rename') return(scopes)


		# Return scopes for directory.resources.features.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.resource.calendar')

		if(function_name == 'directory.resources.features.update') return(scopes)


		# Return scopes for directory.roleAssignments.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roleAssignments.delete') return(scopes)


		# Return scopes for directory.roleAssignments.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly')

		if(function_name == 'directory.roleAssignments.get') return(scopes)


		# Return scopes for directory.roleAssignments.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roleAssignments.insert') return(scopes)


		# Return scopes for directory.roleAssignments.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly')

		if(function_name == 'directory.roleAssignments.list') return(scopes)


		# Return scopes for directory.roles.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roles.delete') return(scopes)


		# Return scopes for directory.roles.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly')

		if(function_name == 'directory.roles.get') return(scopes)


		# Return scopes for directory.roles.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roles.insert') return(scopes)


		# Return scopes for directory.roles.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement',
			'https://www.googleapis.com/auth/admin.directory.rolemanagement.readonly')

		if(function_name == 'directory.roles.list') return(scopes)


		# Return scopes for directory.roles.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roles.patch') return(scopes)


		# Return scopes for directory.roles.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.rolemanagement')

		if(function_name == 'directory.roles.update') return(scopes)


		# Return scopes for directory.schemas.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema')

		if(function_name == 'directory.schemas.delete') return(scopes)


		# Return scopes for directory.schemas.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema',
			'https://www.googleapis.com/auth/admin.directory.userschema.readonly')

		if(function_name == 'directory.schemas.get') return(scopes)


		# Return scopes for directory.schemas.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema')

		if(function_name == 'directory.schemas.insert') return(scopes)


		# Return scopes for directory.schemas.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema',
			'https://www.googleapis.com/auth/admin.directory.userschema.readonly')

		if(function_name == 'directory.schemas.list') return(scopes)


		# Return scopes for directory.schemas.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema')

		if(function_name == 'directory.schemas.patch') return(scopes)


		# Return scopes for directory.schemas.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.userschema')

		if(function_name == 'directory.schemas.update') return(scopes)


		# Return scopes for directory.tokens.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.tokens.delete') return(scopes)


		# Return scopes for directory.tokens.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.tokens.get') return(scopes)


		# Return scopes for directory.tokens.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.tokens.list') return(scopes)


		# Return scopes for directory.twoStepVerification.turnOff
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.twoStepVerification.turnOff') return(scopes)


		# Return scopes for directory.users.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.delete') return(scopes)


		# Return scopes for directory.users.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.readonly')

		if(function_name == 'directory.users.get') return(scopes)


		# Return scopes for directory.users.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.insert') return(scopes)


		# Return scopes for directory.users.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.readonly',
			'https://www.googleapis.com/auth/cloud-platform')

		if(function_name == 'directory.users.list') return(scopes)


		# Return scopes for directory.users.makeAdmin
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.makeAdmin') return(scopes)


		# Return scopes for directory.users.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.patch') return(scopes)


		# Return scopes for directory.users.signOut
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.users.signOut') return(scopes)


		# Return scopes for directory.users.undelete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.undelete') return(scopes)


		# Return scopes for directory.users.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.update') return(scopes)


		# Return scopes for directory.users.watch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.readonly',
			'https://www.googleapis.com/auth/cloud-platform')

		if(function_name == 'directory.users.watch') return(scopes)


		# Return scopes for directory.users.aliases.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.alias')

		if(function_name == 'directory.users.aliases.delete') return(scopes)


		# Return scopes for directory.users.aliases.insert
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.alias')

		if(function_name == 'directory.users.aliases.insert') return(scopes)


		# Return scopes for directory.users.aliases.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.alias',
			'https://www.googleapis.com/auth/admin.directory.user.alias.readonly',
			'https://www.googleapis.com/auth/admin.directory.user.readonly')

		if(function_name == 'directory.users.aliases.list') return(scopes)


		# Return scopes for directory.users.aliases.watch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.alias',
			'https://www.googleapis.com/auth/admin.directory.user.alias.readonly',
			'https://www.googleapis.com/auth/admin.directory.user.readonly')

		if(function_name == 'directory.users.aliases.watch') return(scopes)


		# Return scopes for directory.users.photos.delete
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.photos.delete') return(scopes)


		# Return scopes for directory.users.photos.get
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user',
			'https://www.googleapis.com/auth/admin.directory.user.readonly')

		if(function_name == 'directory.users.photos.get') return(scopes)


		# Return scopes for directory.users.photos.patch
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.photos.patch') return(scopes)


		# Return scopes for directory.users.photos.update
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user')

		if(function_name == 'directory.users.photos.update') return(scopes)


		# Return scopes for directory.verificationCodes.generate
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.verificationCodes.generate') return(scopes)


		# Return scopes for directory.verificationCodes.invalidate
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.verificationCodes.invalidate') return(scopes)


		# Return scopes for directory.verificationCodes.list
		scopes <- c(
			'https://www.googleapis.com/auth/admin.directory.user.security')

		if(function_name == 'directory.verificationCodes.list') return(scopes)
	}
