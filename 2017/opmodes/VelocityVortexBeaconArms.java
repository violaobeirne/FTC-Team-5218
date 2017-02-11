package opmodes;

/*
 * Created by izzielau on 1/2/2017.
 */

import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.AutonomousEvent;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.LimitSwitchTask;
import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;
import test.ModernRoboticsRangeSensorTest;

public class VelocityVortexBeaconArms {

    protected Robot robot;
    protected Servo servo;
    protected ColorSensor color;
    protected DeviceInterfaceModule module;
    protected DeadReckon moveToNextButton;
    protected DeadReckon compensationTurn;
    protected MochaParticleBeaconAutonomous.NumberOfBeacons numberOfBeacons;
    protected boolean isBlueAlliance;
    protected double distanceFromWall;
    protected double target = 0;

    public VelocityVortexBeaconArms(Robot robot, DeviceInterfaceModule interfaceModule, ColorSensor color, DeadReckon moveToNextButton, DeadReckon compensationTurn, Servo servo, boolean isBlueAlliance, MochaParticleBeaconAutonomous.NumberOfBeacons numberOfBeacons, double distanceFromWall)
    {
        this.robot = robot;
        this.servo = servo;
        this.module = interfaceModule;
        this.color = color;
        this.isBlueAlliance = isBlueAlliance;
        this.moveToNextButton = moveToNextButton;
        this.compensationTurn = compensationTurn;
        this.numberOfBeacons = numberOfBeacons;
        this.distanceFromWall = distanceFromWall;
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

        double ticksToMove = ((distanceFromWall - 1) * MochaCalibration.BEACON_TICKS_PER_CM/(float)256.0) + MochaCalibration.BEACON_STOWED_POSITION;
        servo.setPosition(ticksToMove);
        // servo.setPosition(target);

        int seconds = 2000;

        RobotLog.i("163 Deploying servo for " + seconds + " seconds");
        robot.addTask(new SingleShotTimerTask(robot, seconds) {
            @Override
            public void handleEvent(RobotEvent e) {
                stowServo();
            }
        });
    }

    public void redeployServo()
    {
        robot.addTask(new SingleShotTimerTask(robot, 5000) {
            @Override
            public void handleEvent(RobotEvent e) {
                deployServo();
            }
        });
    }

    public void stowServo()
    {
        RobotLog.i("163 Double-checking the beacon because our color sensor sucks");
        moveToSecondBeacon();
        /*
        robot.addTask(new ColorSensorTask(robot, color, module, false, false, 0) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent event = (ColorSensorEvent) e;
                if (event.kind == EventKind.BLUE && !isBlueAlliance) {
                    servo.setPosition(MochaCalibration.BEACON_STOWED_POSITION);
                    redeployServo();
                } else if (event.kind == EventKind.RED && isBlueAlliance) {
                    servo.setPosition(MochaCalibration.BEACON_STOWED_POSITION);
                    redeployServo();
                }
                moveToSecondBeacon();
            }
        });
        */
    }

    public void moveToSecondBeacon()
    {
        RobotLog.i("163 Timer start, stowing the servo");
        servo.setPosition(MochaCalibration.BEACON_STOWED_POSITION);

        if (distanceFromWall >= 20) {
            RobotLog.i("163 Adding compensation, distance from wall: " + distanceFromWall);
            robot.addTask(new DeadReckonTask(robot, compensationTurn) {
                @Override
                public void handleEvent(RobotEvent e) {
                    if (numberOfBeacons == MochaParticleBeaconAutonomous.NumberOfBeacons.TWO) {
                        firstBeaconWorkDone();
                    }
                }
            });
        } else {
            RobotLog.i("163 Prepping for second beacon, distance from wall: " + distanceFromWall);
            if (numberOfBeacons == MochaParticleBeaconAutonomous.NumberOfBeacons.TWO) {
                firstBeaconWorkDone();
            }
        }
    }

    public void firstBeaconWorkDone()
    {
        RobotLog.i("163 Queuing BeaconDone AutonomousEvent");
        AutonomousEvent beaconDone = new AutonomousEvent(robot, AutonomousEvent.EventKind.BEACON_DONE);
        robot.queueEvent(beaconDone);
    }
}