package alpha.portal.dao;

import org.appfuse.dao.GenericDao;

import alpha.portal.model.ContributorRole;

public interface ContributorRoleDao extends GenericDao<ContributorRole, Long> {
    
    public ContributorRole getContributorRoleByName(String name);
}
