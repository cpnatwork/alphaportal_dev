package alpha.portal.service;

import java.util.List;

import org.appfuse.service.GenericManager;

import alpha.portal.model.ContributorRole;
import alpha.portal.model.UserExtension;

/**
 * Interface UserExtensionManager, which extends from the GenericManager with following objects: UserExtension and Long.
 */
public interface UserExtensionManager extends GenericManager<UserExtension, Long> {

    /**
     * Get all users that have the specific contributor role
     * 
     * @param role
     *            the given contributor role
     * @return List with UserExtensions
     */
    public List<UserExtension> getUserExtensionsByContributorRole(ContributorRole role);
}
