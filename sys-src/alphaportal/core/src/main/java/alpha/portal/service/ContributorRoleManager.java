package alpha.portal.service;

import org.appfuse.service.GenericManager;

import alpha.portal.model.ContributorRole;

/**
 * Interface ContributorRoleManager, which extends from the GenericManager with
 * following objects: UserExtension and Long.
 */
public interface ContributorRoleManager extends GenericManager<ContributorRole, Long> {

    /**
     * Get a ContributorRole by name.
     * 
     * @param name
     *            the name of the ContributorRole
     * @return the ContributorRole or null
     */
    public ContributorRole getContributorRoleByName(String name);
}
