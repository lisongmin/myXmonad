myXmonad
========

* Install xmonad config with
```bash
git clone https://github.com/lisongmin/myXmonad.git ~/.xmonad
```

* change conkyrc with your network interface
```
sed -i 's/enp4s0f1/youre_wired_nic/g' conkyrc
sed -i 's/wlp3s0/your_wireless_nic/g' conkyrc
```

* Generate xmonad with
```bash
xmonad --recompile
```
