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
package alpha.portal.webapp.search;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.compass.core.config.CompassConfiguration;
import org.compass.core.config.ConfigurationException;
import org.compass.spring.LocalCompassBeanPostProcessor;

/**
 * Compass Post Processor that allows for adding scan mappings for more than one
 * root package.
 */
public class CompassConfigurationPostProcessor implements
		LocalCompassBeanPostProcessor {

	/** The log. */
	Log log = LogFactory.getLog(CompassConfigurationPostProcessor.class);

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.compass.spring.LocalCompassBeanPostProcessor#process(org.compass.
	 * core.config.CompassConfiguration)
	 */
	public void process(final CompassConfiguration config)
			throws ConfigurationException {
		// Look at current class's package and add it if it's not the AppFuse
		// default
		final String classPackage = this.getClass().getPackage().getName();
		final String rootPackage = classPackage.substring(0,
				classPackage.indexOf("webapp") - 1);
		if (!rootPackage.equals("org.appfuse")) {
			this.log.debug("Adding scan for package: " + rootPackage);
			config.addScan(rootPackage);
		}
	}
}
