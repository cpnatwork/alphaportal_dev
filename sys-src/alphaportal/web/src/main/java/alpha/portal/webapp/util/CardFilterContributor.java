package alpha.portal.webapp.util;

public enum CardFilterContributor {
    ALL("--Verantwortlicher"), OWN("eigene"), OTHERS("andere");

    private String name;

    private CardFilterContributor(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
