/**************************************************************************
 * alpha-Portal: A web portal, for managing knowledge-driven 
 * ad-hoc processes, in form of case files.
 * ==============================================
 * Copyright (C) 2011-2012 by 
 *   - Christoph P. Neumann (http://www.chr15t0ph.de)
 *   - and the SWAT 2011 team
 **************************************************************************
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *     http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, 
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and 
 * limitations under the License.
 **************************************************************************
 * $Id$
 *************************************************************************/
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
public class PayloadDaoHibernate extends
		GenericDaoHibernate<Payload, PayloadIdentifier> implements PayloadDao {

	/**
	 * Instantiates a new user extension dao hibernate.
	 */
	public PayloadDaoHibernate() {
		super(Payload.class);
	}

	/**
	 * Gets the all versions.
	 * 
	 * @param payload
	 *            the payload
	 * @return the all versions
	 * @see alpha.portal.dao.PayloadDao#getAllVersions()
	 */
	@SuppressWarnings("unchecked")
	public List<Payload> getAllVersions(final Payload payload) {

		final Long payloadId = payload.getPayloadIdentifier().getPayloadId();

		return this
				.getHibernateTemplate()
				.find("from payload where payloadIdentifier.payloadId = ? order by payloadIdentifier.sequenceNumber desc",
						payloadId);
	}

	/**
	 * Gets the version.
	 * 
	 * @param id
	 *            the id
	 * @return the version
	 * @see alpha.portal.dao.PayloadDao#getVersion(int)
	 */
	public Payload getVersion(final PayloadIdentifier id) {
		return (Payload) this
				.getHibernateTemplate()
				.find("from payload where payloadIdentifier.payloadId = ? and payloadIdentifier.sequenceNumber = ?",
						id.getPayloadId(), id.getSequenceNumber()).get(0);
	}

	/**
	 * Save.
	 * 
	 * @param payload
	 *            the payload
	 * @return the payload
	 * @see org.appfuse.service.impl.GenericManagerImpl#save(java.lang.Object)
	 */
	@Override
	public Payload save(final Payload payload) {
		if (payload.getPayloadIdentifier().getSequenceNumber() != 0) {
			this.getHibernateTemplate().flush();
			this.getHibernateTemplate().evict(payload);
			this.getHibernateTemplate().clear();
		}

		final PayloadIdentifier id = payload.getPayloadIdentifier();
		if (id.getPayloadId() == 0) {
			id.setPayloadId(this.getLastValue(this.getSessionFactory(),
					"payloadId") + 1);
		}
		id.setSequenceNumber(this.getLastValue(this.getSessionFactory(),
				"sequenceNumber") + 1L);

		payload.setPayloadIdentifier(id);
		return super.save(payload);
	}

	/**
	 * Internal function to load the highest sequenceNumber from the AlphaCard
	 * table.
	 * 
	 * @param sessionFactory
	 *            the session factory
	 * @param column
	 *            the column
	 * @return 0 if no record is found
	 */
	private Long getLastValue(final SessionFactory sessionFactory,
			final String column) {
		Session session;
		boolean sessionOwn = false;
		try {
			session = sessionFactory.getCurrentSession();
		} catch (final Exception e) {
			session = sessionFactory.openSession();
			sessionOwn = true;
		}
		final Query q = session.createSQLQuery("select max(" + column
				+ ") from payload");
		final List<Object> list = q.list();
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
