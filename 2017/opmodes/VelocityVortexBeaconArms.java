package opmodes;

/*
 * Created by izzielau on 1/2/2017.
 */

import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.DeadReckonTask;
import team25core.LimitSwitchTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

public class VelocityVortexBeaconArms {

    public enum ServoType {
        CONTINUOUS,
        STANDARD,
    }

    protected Robot robot;
    protected Servo servo;
    protected ServoType servoType;
    protected DeviceInterfaceModule module;
    protected DeadReckon moveToNextButton;
    protected boolean isBlueAlliance;
    protected double deployPosition;
    protected double stowPosition;

    protected int RIGHT_PORT = MochaCalibration.RIGHT_LIMIT_PORT;
    protected double RIGHT_DIRECTION = MochaCalibration.RIGHT_PRESSER_DIRECTION;
    protected int LEFT_PORT = MochaCalibration.LEFT_LIMIT_PORT;
    protected double LEFT_DIRECTION = MochaCalibration.LEFT_PRESSER_DIRECTION;

    protected int killTimer = 0;

    public VelocityVortexBeaconArms(Robot robot, DeviceInterfaceModule interfaceModule, DeadReckon moveToNextButton, Servo servo, ServoType servoType, boolean isBlueAlliance)
    {
        this.robot = robot;
        this.servo = servo;
        this.servoType = servoType;
        this.module = interfaceModule;
        this.isBlueAlliance = isBlueAlliance;
        this.moveToNextButton = moveToNextButton;
    }

    public VelocityVortexBeaconArms(Robot robot, DeviceInterfaceModule interfaceModule, DeadReckon moveToNextButton, Servo servo, ServoType servoType, double deploy, double stow, boolean isBlueAlliance)
    {
        this.robot = robot;
        this.servo = servo;
        this.servoType = servoType;
        this.module = interfaceModule;
        this.isBlueAlliance = isBlueAlliance;
        this.moveToNextButton = moveToNextButton;
        this.stowPosition = stow;
        this.deployPosition = deploy;
    }


    public void deploy(boolean sensedMyAlliance, boolean firstButton)
    {
        if (sensedMyAlliance && isBlueAlliance) {
            RobotLog.i("163 Blue alliance, aligned correctly");
            deployServo(RIGHT_PORT, RIGHT_DIRECTION);
        } else if (sensedMyAlliance && !isBlueAlliance){
            RobotLog.i("163 Red alliance, aligned correctly");
            deployServo(LEFT_PORT, LEFT_DIRECTION);
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
                    if (isBlueAlliance) {
                        deploy(true, false);
                    } else {
                        deploy(true, false);
                    }
                }
            }
        });
    }

    public void deployServo(int limitPort, final double direction)
    {
        RobotLog.i("163 Limit switch closed or kill timer done, stopping servo");
        switch (servoType) {
            case CONTINUOUS:
                robot.addTask(new LimitSwitchTask(robot, module, limitPort) {
                    @Override
                    public void handleEvent(RobotEvent e)
                    {
                        LimitSwitchEvent event = (LimitSwitchEvent) e;

                        servo.setPosition(direction);
                        if ((event.kind == EventKind.CLOSED) || (killTimer >= 7)) {
                            RobotLog.i("163 Limit switch closed or kill timer done, stopping servo");
                            stowServo();
                        } else {
                            runTimer();
                        }
                    }
                });
                break;
            case STANDARD:
                robot.addTask(new SingleShotTimerTask(robot, 2000) {
                    @Override
                    public void handleEvent(RobotEvent e) {
                        servo.setPosition(deployPosition);
                    }
                });
                break;
        }
    }

    public void runTimer()
    {
        robot.addTask(new SingleShotTimerTask(robot, 1000) {
            @Override
            public void handleEvent(RobotEvent e) {
                SingleShotTimerEvent event = (SingleShotTimerEvent) e;

                if (event.kind == EventKind.EXPIRED) {
                    RobotLog.i("163 Increasing kill timer %d", killTimer);
                    killTimer++;
                }
            }
        });
    }

    public void stowServo()
    {
        RobotLog.i("163 Timer start, stowing the servo");
        if (isBlueAlliance) {
            servo.setPosition(LEFT_DIRECTION);
        } else {
            servo.setPosition(RIGHT_DIRECTION);
        }

        robot.addTask(new SingleShotTimerTask(robot, 1000) {
            @Override
            public void handleEvent(RobotEvent e) {
                SingleShotTimerEvent event = (SingleShotTimerEvent) e;

                if (event.kind == EventKind.EXPIRED) {
                    RobotLog.i("163 Timer done, finish stowing the servo");

                    if (servoType == ServoType.CONTINUOUS) {
                        servo.setPosition(0.5);
                    }
                }
            }
        });
    }
}