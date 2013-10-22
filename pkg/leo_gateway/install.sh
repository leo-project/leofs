#!/usr/bin/bash

USER=leofs
GROUP=$USER
COMPONENT=leo_gateway
DIR=/opt/local/$COMPONENT

case $2 in
    PRE-INSTALL)
        if grep "^$GROUP:" /etc/group > /dev/null 2>&1
        then
            echo "Group already exists, skipping creation."
        else
            echo Creating $GROUP group ...
            groupadd $GROUP
        fi
        if id $USER > /dev/null 2>&1
        then
            echo "User already exists, skipping creation."
        else
            echo Creating $USER user ...
            useradd -g $GROUP -d /var/db/$COMPONENT -s /bin/false $USER
        fi
        echo Creating directories ...
        mkdir -p /var/db/$COMPONENT
        chown -R $USER:$GROUP /var/db/$COMPONENT
        mkdir -p /var/log/$COMPONENT/sasl
        chown -R $USER:$GROUP /var/log/$COMPONENT
        if [ -d /tmp/$COMPONENT ]
        then
            chown -R $USER:$GROUP /tmp/$COMPONENT/
        fi
        ;;
    POST-INSTALL)
        if svcs svc:/network/$COMPONENT:default > /dev/null 2>&1
        then
            echo Service already existings ...
        else
            echo Importing service ...
            svccfg import /opt/local/$COMPONENT/share/$COMPONENT.xml
        fi
        echo Trying to guess configuration ...
        IP=`ifconfig net0 | grep inet | awk -e '{print $2}'`
        if [ ! -f /opt/local/$COMPONENT/etc/vm.args ]
        then
            cp /opt/local/$COMPONENT/etc/vm.args.example /opt/local/$COMPONENT/etc/vm.args
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/$COMPONENT/etc/vm.args
        fi
        if [ ! -f /opt/local/$COMPONENT/etc/app.config ]
        then
            cp /opt/local/$COMPONENT/etc/app.config.example /opt/local/$COMPONENT/etc/app.config
            sed --in-place -e "s/127.0.0.1/${IP}/g" /opt/local/$COMPONENT/etc/app.config
        fi
        echo "Checking for $DIR/bin/$COMPONENT.smartos"
        if [ ! -f $DIR/bin/$COMPONENT.smartos ]
        then
            echo "Installing smartos binary"
            cp  $DIR/share/$COMPONENT.smartos $DIR/bin/
            cp  $DIR/share/$COMPONENT.smartos $DIR/bin/$COMPONENT
        fi    

        echo "Checking for $DIR/etc/app.config.smartos"
        if [ ! -f $DIR/etc/app.config.smartos ]
        then
            echo "Installing smartos config"
            cp  $DIR/share/app.config.smartos $DIR/etc/
            cp  $DIR/etc/app.config.smartos $DIR/etc/app.config
            sed --in-place -e "s/127.0.0.1/${IP}/g" $DIR/etc/app.config
        fi    
        ;;
esac
