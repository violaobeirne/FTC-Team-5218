package opmodes;

/*
 * Created by izzielau on 1/2/2017.
 */

import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.AutonomousEvent;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.LimitSwitchTask;
import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

public class VelocityVortexBeaconArms {

    protected Robot robot;
    protected Servo servo;
    protected DeviceInterfaceModule module;
    protected DeadReckon moveToNextButton;
    protected MochaParticleBeaconAutonomous.NumberOfBeacons numberOfBeacons;
    protected boolean isBlueAlliance;
    protected double smartStow;
    protected double target = 0;

    public VelocityVortexBeaconArms(Robot robot, DeviceInterfaceModule interfaceModule, DeadReckon moveToNextButton, Servo servo, boolean isBlueAlliance, MochaParticleBeaconAutonomous.NumberOfBeacons numberOfBeacons, double smartStow)
    {
        this.robot = robot;
        this.servo = servo;
        this.module = interfaceModule;
        this.isBlueAlliance = isBlueAlliance;
        this.moveToNextButton = moveToNextButton;
        this.numberOfBeacons = numberOfBeacons;
        this.smartStow = smartStow;
    }

    public VelocityVortexBeaconArms(Robot robot, DeviceInterfaceModule interfaceModule, DeadReckon moveToNextButton, Servo servo, boolean isBlueAlliance)
    {
        this.robot = robot;
        this.servo = servo;
        this.module = interfaceModule;
        this.isBlueAlliance = isBlueAlliance;
        this.moveToNextButton = moveToNextButton;
    }

    public void deploy(boolean sensedMyAlliance, boolean firstButton)
    {
        if (sensedMyAlliance && isBlueAlliance) {
            RobotLog.i("163 Blue alliance, aligned correctly");
            deployServo();
        } else if (sensedMyAlliance && !isBlueAlliance){
            RobotLog.i("163 Red alliance, aligned correctly");
            deployServo();
        } else if (!sensedMyAlliance && firstButton) {
            RobotLog.i("163 Wrong color, moving on to the next button");
            handleWrongColor();
        } else {
            RobotLog.e("163 Something went wrong with the color sensing");
        }
    }

    public void handleWrongColor()
    {
        robot.addTask(new DeadReckonTask(robot, moveToNextButton){
            @Override
            public void handleEvent(RobotEvent e)
            {
                DeadReckonEvent event = (DeadReckonEvent) e;
                if (event.kind == EventKind.PATH_DONE) {
                    RobotLog.i("163 Robot finished moving to the next button, redoing color");
                    deploy(true, false);
                }
            }
        });
    }

    public void deployServo()
    {
        target = (smartStow + MochaCalibration.BEACON_STOWED_POSITION + (6 * MochaCalibration.BEACON_TICKS_PER_CM/(float)256.0));
        RobotLog.i("163 Timer start, moving the servo to " + target);

        servo.setPosition(target);
        robot.addTask(new SingleShotTimerTask(robot, 3000) {
            @Override
            public void handleEvent(RobotEvent e) {
                stowServo();
            }
        });
    }

    public void stowServo()
    {
        RobotLog.i("163 Timer start, stowing the servo");
        servo.setPosition(MochaCalibration.BEACON_STOWED_POSITION);
        if (numberOfBeacons == MochaParticleBeaconAutonomous.NumberOfBeacons.TWO) {
            firstBeaconWorkDone();
        }
    }

    public void firstBeaconWorkDone()
    {
        RobotLog.i("163 Queuing BeaconDone AutonomousEvent");
        AutonomousEvent beaconDone = new AutonomousEvent(robot, AutonomousEvent.EventKind.BEACON_DONE);
        robot.queueEvent(beaconDone);
    }
}