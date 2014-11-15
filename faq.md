

# NetworkManager can not start wireless by manual

solution is [set up PolcyKit permissions](https://wiki.archlinux.org/index.php/NetworkManager#Set_up_PolicyKit_permissions)

* add yourself to network group
```bash
sudo usermod -a -G network $USER
```
* add polkit rule file `/etc/polkit-1/rules.d/50-org.freedesktop.NetworkManager.rules`
```
polkit.addRule(function(action, subject) {
  if (action.id.indexOf("org.freedesktop.NetworkManager.") == 0 && subject.isInGroup("network")) {
    return polkit.Result.YES;
  }
});
```
The polkit actions can found in `/usr/share/polkit-1`

