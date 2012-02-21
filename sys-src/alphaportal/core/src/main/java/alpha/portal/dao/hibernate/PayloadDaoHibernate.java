package alpha.portal.dao.hibernate;

import java.math.BigInteger;
import java.util.List;

import org.appfuse.dao.hibernate.GenericDaoHibernate;
import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.hibernate.classic.Session;
import org.springframework.stereotype.Repository;

import alpha.portal.dao.PayloadDao;
import alpha.portal.model.Payload;
import alpha.portal.model.PayloadIdentifier;

/**
 * The PayloadDaoHibernate implements the interface Payload.
 * 
 * @see PayloadDao PayloadDAO
 */
@Repository("payloadDao")
public class PayloadDaoHibernate extends GenericDaoHibernate<Payload, PayloadIdentifier> implements PayloadDao {

    /**
     * Instantiates a new user extension dao hibernate.
     */
    public PayloadDaoHibernate() {
        super(Payload.class);
    }

    /**
     * @see alpha.portal.dao.PayloadDao#getAllVersions()
     */
    @SuppressWarnings("unchecked")
    public List<Payload> getAllVersions(final Payload payload) {

        Long payloadId = payload.getPayloadIdentifier().getPayloadId();

        return getHibernateTemplate().find(
                "from payload where payloadIdentifier.payloadId = ? order by payloadIdentifier.sequenceNumber desc",
                payloadId);
    }

    /**
     * @see alpha.portal.dao.PayloadDao#getVersion(int)
     */
    public Payload getVersion(final PayloadIdentifier id) {
        return (Payload) getHibernateTemplate().find(
                "from payload where payloadIdentifier.payloadId = ? and payloadIdentifier.sequenceNumber = ?",
                id.getPayloadId(), id.getSequenceNumber()).get(0);
    }

    /**
     * @see org.appfuse.service.impl.GenericManagerImpl#save(java.lang.Object)
     */
    @Override
    public Payload save(final Payload payload) {
        if (payload.getPayloadIdentifier().getSequenceNumber() != 0) {
            getHibernateTemplate().flush();
            getHibernateTemplate().evict(payload);
            getHibernateTemplate().clear();
        }

        PayloadIdentifier id = payload.getPayloadIdentifier();
        if (id.getPayloadId() == 0) {
            id.setPayloadId(getLastValue(getSessionFactory(), "payloadId") + 1);
        }
        id.setSequenceNumber(getLastValue(getSessionFactory(), "sequenceNumber") + 1L);

        payload.setPayloadIdentifier(id);
        return super.save(payload);
    }

    /**
     * Internal function to load the highest sequenceNumber from the AlphaCard table.
     * 
     * @return 0 if no record is found
     */
    private Long getLastValue(final SessionFactory sessionFactory, final String column) {
        Session session;
        boolean sessionOwn = false;
        try {
            session = sessionFactory.getCurrentSession();
        } catch (Exception e) {
            session = sessionFactory.openSession();
            sessionOwn = true;
        }
        Query q = session.createSQLQuery("select max(" + column + ") from payload");
        List<Object> list = q.list();
        BigInteger value = (BigInteger) list.get(0);
        if (value == null) {
            value = new BigInteger("0");
        }
        if (sessionOwn) {
            session.close();
        }
        return value.longValue();
    }
}
