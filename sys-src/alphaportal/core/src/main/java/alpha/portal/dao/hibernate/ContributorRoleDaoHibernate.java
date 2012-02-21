package alpha.portal.dao.hibernate;

import java.util.List;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.ContributorRoleDao;
import alpha.portal.model.ContributorRole;

@Repository("contributorRoleDao")
public class ContributorRoleDaoHibernate extends GenericDaoHibernate<ContributorRole, Long> implements
        ContributorRoleDao {

    public ContributorRoleDaoHibernate() {
        super(ContributorRole.class);
    }

    @SuppressWarnings("unchecked")
    public ContributorRole getContributorRoleByName(String name) {
        List<ContributorRole> results = getHibernateTemplate().find("from contributorrole where name=?", name);
        ContributorRole cRole = results.isEmpty() ? null : results.get(0);
        return cRole;
    }


}
