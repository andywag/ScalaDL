/*******************************************************************************
 * Copyright (c) 2007/2008 Simplifide, LLC.
 * All Right Reserved.
 ******************************************************************************/
package com.simplifide.base;


public class BaseLog {
	
	
	public static boolean debugInfo = false;
	public static boolean debugParse = false;
	/**
	    * Log the specified information.
	    * 
	    * @param message, a human-readable message, localized to the
	    *           current locale.
	    */
	
	
	   public static void logInfo(String message) {
               //if (!BaseActivator.RELEASE)
               //    log(IStatus.INFO, IStatus.OK, message, null);
	   }
           
           public static void logInfoAlways(String message) {
              // log(IStatus.INFO, IStatus.OK, message, null);
           }

	   /**
	    * Log the specified error.
	    * 
	    * @param exception, a low-level exception.
	    */
	   public static void logError(Throwable exception) {
	       if (debugInfo)
                   logError("Unexpected Exception", exception);
	   }
	   
	   public static void logErrorString(String e, Throwable exception) {
	       if (debugInfo)
                   logError(e, exception);
	   }
	   
	   public static void logParseError(String message, Throwable exception) {
		   System.out.println(message); 
           exception.printStackTrace(System.out);
           //System.out.println(exception.getStackTrace());
		   //if (debugParse)
               //log(IStatus.ERROR, IStatus.OK, message, exception);
	   }

	   /**
	    * Log the specified error.
	    * 
	    * @param message, a human-readable message, localized to the
	    *           current locale.
	    * @param exception, a low-level exception, or <code>null</code>
	    *           if not applicable.
	    */
	   public static void logError(String message, Throwable exception) {
             //  if (!BaseActivator.RELEASE)
             //      log(IStatus.ERROR, IStatus.OK, message, exception);
	   }

	   /**
	    * Log the specified information.
	    * 
	    * @param severity, the severity; one of the following:
	    *           <code>IStatus.OK</code>,
	    *           <code>IStatus.ERROR</code>,
	    *           <code>IStatus.INFO</code>, or
	    *           <code>IStatus.WARNING</code>.
	    * @param pluginId. the unique identifier of the relevant
	    *           plug-in.
	    * @param code, the plug-in-specific status code, or
	    *           <code>OK</code>.
	    * @param message, a human-readable message, localized to the
	    *           current locale.
	    * @param exception, a low-level exception, or <code>null</code>
	    *           if not applicable.
	    */
	   public static void log(int severity, int code, String message,
	         Throwable exception) {
	           //log(createStatus(severity, code, message, exception));
	   }




}
