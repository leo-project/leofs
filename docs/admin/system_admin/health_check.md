# Health Check
## Health Check URL
You can use the health check URL **/_leofs_adm/ping** in case you put Load Balancer(LB) in front of LeoGateway's.
This URL enables you to set up your LB as easy as just pasting the URL into somewhere in your LB configuration rather than dealing with complex works described on [LEOFS' ISSUE_858](https://github.com/leo-project/leofs/issues/858) in order to pass the authorization in S3 mode. You can use the URL in REST mode as well.

!!! note "Note: Health Check in REST mode"
    You can't get an object stored at **/_leofs_adm/ping** as it's shadowed by the Health Check URL so be careful not to put any object at **/_leofs_adm/ping** in REST mode.

## Related Links

- [LEOFS' ISSUE_858](https://github.com/leo-project/leofs/issues/858)
- [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)
