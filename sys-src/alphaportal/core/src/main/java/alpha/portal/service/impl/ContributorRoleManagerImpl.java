package alpha.portal.service.impl;

import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.ContributorRoleDao;
import alpha.portal.model.ContributorRole;
import alpha.portal.service.ContributorRoleManager;

@Service("contributorRoleManager")
public class ContributorRoleManagerImpl extends GenericManagerImpl<ContributorRole, Long> implements
        ContributorRoleManager {

    private ContributorRoleDao contributorRoleDao;

    @Autowired
    public ContributorRoleManagerImpl(final ContributorRoleDao contributorRoleDao) {
        super(contributorRoleDao);
        this.contributorRoleDao = contributorRoleDao;
    }

    public ContributorRole getContributorRoleByName(String name) {
        return contributorRoleDao.getContributorRoleByName(name);
    }
}
