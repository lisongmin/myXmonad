

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

# automount under xmonad
## 采用 udisks2 + udiskie 完成外接硬盘的自动挂载。
```bash
pacman -S udisks2 udiskie
```
## 配置udisks2的访问权限
* 加入storage组
```bash
usermod -a -G storage $USER
```
需要注销才能生效，目前还不知道怎么立即生效。

* 增加polkit规则
增加 /etc/polkit-1/rules.d/50-udisks.rules 文件，加入以下内容：
```
polkit.addRule(function(action, subject) {
  var YES = polkit.Result.YES;
  var permission = {
    // only required for udisks1:
    "org.freedesktop.udisks.filesystem-mount": YES,
    "org.freedesktop.udisks.filesystem-mount-system-internal": YES,
    "org.freedesktop.udisks.luks-unlock": YES,
    "org.freedesktop.udisks.drive-eject": YES,
    "org.freedesktop.udisks.drive-detach": YES,
    // only required for udisks2:
    "org.freedesktop.udisks2.filesystem-mount": YES,
    "org.freedesktop.udisks2.filesystem-mount-system": YES,
    "org.freedesktop.udisks2.encrypted-unlock": YES,
    "org.freedesktop.udisks2.eject-media": YES,
    "org.freedesktop.udisks2.power-off-drive": YES
  };
  if (subject.isInGroup("storage")) {
    return permission[action.id];
  }
});
```
* 启动xmonad的时候，启动udiskie，参见xmonad.hs
