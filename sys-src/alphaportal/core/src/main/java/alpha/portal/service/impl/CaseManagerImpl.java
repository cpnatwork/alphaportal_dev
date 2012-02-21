package alpha.portal.service.impl;

import java.util.LinkedList;
import java.util.List;

import org.appfuse.model.User;
import org.appfuse.service.impl.GenericManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import alpha.portal.dao.AlphaCaseDao;
import alpha.portal.model.AlphaCard;
import alpha.portal.model.AlphaCase;
import alpha.portal.service.CaseManager;
import alpha.portal.service.CaseService;

/**
 * implements the interface AlphaCaseManager.
 * 
 * @see AlphaCaseManager α-Case Manager
 */
@Service("caseManager")
public class CaseManagerImpl extends GenericManagerImpl<AlphaCase, String> implements CaseManager, CaseService {

    private AlphaCaseDao caseDao;

    @Autowired
    public CaseManagerImpl(final AlphaCaseDao caseDao) {
        super(caseDao);
        this.caseDao = caseDao;
    }

    public List<AlphaCase> findByName(final String name) {
        return caseDao.findByName(name);
    }

    public List<AlphaCase> findByParticipant(final User user) {
        return caseDao.findByParticipant(user);
    }

    /**
     * @see alpha.portal.service.CaseManager#updateCardOrder(alpha.portal.model.AlphaCase, java.util.List)
     */
    public void updateCardOrder(final AlphaCase alphaCase, final List<String> cardIds) {
        if (cardIds.size() < alphaCase.getAlphaCards().size())
            throw new IllegalArgumentException("Not all AlphaCards are in this order-set!");
        List<AlphaCard> newCards = new LinkedList<AlphaCard>();
        for (String cardId : cardIds) {
            newCards.add(alphaCase.getAlphaCasePSA().getAlphaCardByCardId(cardId));
        }
        alphaCase.getAlphaCasePSA().setAlphaCards(newCards);
        this.save(alphaCase);
    }

    /**
     * @see alpha.portal.service.CaseManager#removeAlphaCard(alpha.portal.model.AlphaCard)
     */
    public void removeAlphaCard(final AlphaCard card) {
        AlphaCase aCase = card.getAlphaCase();
        aCase.removeAlphaCard(card);
        this.save(aCase);
    }

    /**
     * {@inheritDoc}
     * 
     * @see CaseService#getCase(String)
     */
    public AlphaCase getCase(final String caseId) {
        return get(caseId);
    }

    /**
     * {@inheritDoc}
     * 
     * @see CaseService#getCases()
     */
    public List<AlphaCase> getCases() {
        return getAll();
    }

    /**
     * {@inheritDoc}
     * 
     * @see CaseService#removeCase(String)
     */
    public void removeCase(final String caseId) {
        remove(caseId);
    }

    /**
     * {@inheritDoc}
     * 
     * @see CaseService#saveCase(AlphaCase)
     */
    public AlphaCase saveCase(final AlphaCase alphaCase) {
        return save(alphaCase);
    }
}