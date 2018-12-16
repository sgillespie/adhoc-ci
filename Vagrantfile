# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  # Use the latest NixOS box. To use this, you'll have to manually build
  # it from https://github.com/nix-community/nixbox
  config.vm.box = "nixos/nixos-18.09-x86_64"

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  config.vm.network "private_network", type: "dhcp"

  # Provider-specific configuration
  config.vm.provider "virtualbox" do |vb|
    # vb.gui = true
    vb.memory = "1024"
  end

  # Enable provisioning with a nixos configuration. This requires the NixOS
  # plugin: https://github.com/nix-community/vagrant-nixos-plugin
  config.vm.provision :nixos,
    run: 'always',
    expression: {
      virtualisation: {
        docker: {
          enable: true,
          enableOnBoot: true,
          liveRestore: false,
          listenOptions: ["/var/run/docker.sock", "9990"]
        }
      },

      networking: {
        firewall: {
          enable: false
        }
      },
      
      services: {
        openssh: {
          enable: true
        }
      }
    }

  # Create a master node
  config.vm.define "master" do |master|
    master.vm.network "forwarded_port", guest: 9990, host: 9990
  end

  # And a worker
  config.vm.define "worker-1" do |worker|
    worker.vm.network "forwarded_port", guest: 9990, host: 9991
  end
end
