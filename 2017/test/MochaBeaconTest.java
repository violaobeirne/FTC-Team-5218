package test;/*
 * Created by izzielau on 1/31/2017.
 */

import com.qualcomm.hardware.modernrobotics.ModernRoboticsI2cRangeSensor;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DeviceInterfaceModule;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import opmodes.MochaCalibration;
import opmodes.MochaParticleBeaconAutonomous;
import opmodes.VelocityVortexBeaconArms;
import team25core.ColorSensorTask;
import team25core.DeadReckon;
import team25core.FourWheelDirectDriveDeadReckon;
import team25core.PeriodicTimerTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

@Autonomous(name = "TEST Beacon", group = "TEST")
public class MochaBeaconTest extends Robot {

    MochaParticleBeaconAutonomous.NumberOfBeacons numberOfBeacons;
    FourWheelDirectDriveDeadReckon moveToNextButton;
    VelocityVortexBeaconArms beaconArms;

    DcMotor frontRight;
    DcMotor frontLeft;
    DcMotor backRight;
    DcMotor backLeft;

    DeviceInterfaceModule interfaceModule;
    ColorSensor color;
    ModernRoboticsI2cRangeSensor rangeSensor;
    Servo beacon;

    private boolean isBlueAlliance = false;
    private double distanceFromWall;
    private double ticksToMove;

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void init()
    {
        interfaceModule = hardwareMap.deviceInterfaceModule.get("interface");

        beacon = hardwareMap.servo.get("beacon");

        rangeSensor = hardwareMap.get(ModernRoboticsI2cRangeSensor.class, "rangeSensor");
        color = hardwareMap.colorSensor.get("color");

        frontRight = hardwareMap.dcMotor.get("motorFR");
        frontLeft = hardwareMap.dcMotor.get("motorFL");
        backRight = hardwareMap.dcMotor.get("motorBR");
        backLeft = hardwareMap.dcMotor.get("motorBL");

        beacon.setPosition(MochaCalibration.BEACON_STOWED_POSITION);
        numberOfBeacons = MochaParticleBeaconAutonomous.NumberOfBeacons.ONE;
    }

    @Override
    public void start()
    {
        addTask(new PeriodicTimerTask(this, 40){
            @Override
            public void handleEvent(RobotEvent e) {
                distanceFromWall = rangeSensor.getDistance(DistanceUnit.CM);
                RobotLog.i("163 Range sensor distance %f", distanceFromWall);
                if (distanceFromWall != 255) {
                    this.stop();
                    checkForDistanceFromWallUsingMRRange();
                }
            }
        });
    }

    protected void checkForDistanceFromWallUsingMRRange()
    {
        RobotLog.i("163 Distance from wall: " + distanceFromWall);

        if (distanceFromWall > MochaCalibration.RANGE_DISTANCE_MINIMUM) {
            ticksToMove = (((distanceFromWall - 12) * MochaCalibration.BEACON_TICKS_PER_CM)/(float)256.0);
            RobotLog.i("163 Moving the servo to color read position " + (ticksToMove + MochaCalibration.BEACON_STOWED_POSITION));
            beacon.setPosition(ticksToMove + MochaCalibration.BEACON_STOWED_POSITION);
        }

        addTask(new SingleShotTimerTask(this, 3000) {
            @Override
            public void handleEvent(RobotEvent e)
            {
                SingleShotTimerEvent event = (SingleShotTimerEvent) e;
                if (event.kind == EventKind.EXPIRED) {
                    handleAlignedWithColor();
                }
            }
        });
    }

    protected void handleAlignedWithColor()
    {
        moveToNextButton = new FourWheelDirectDriveDeadReckon
                (this, MochaCalibration.TICKS_PER_INCH, MochaCalibration.TICKS_PER_DEGREE, frontRight, backRight, frontLeft, backLeft);
        moveToNextButton.addSegment(DeadReckon.SegmentType.STRAIGHT, 4, 0.5 * -MochaCalibration.MOVE_SPEED);

        final double smartStowValue = ticksToMove;

        beaconArms = new VelocityVortexBeaconArms(this, interfaceModule, moveToNextButton, beacon, isBlueAlliance, numberOfBeacons, smartStowValue);
        addTask(new ColorSensorTask(this, color, interfaceModule, false, true, 0) {
            @Override
            public void handleEvent(RobotEvent e) {
                ColorSensorEvent color = (ColorSensorEvent) e;

                if (isBlueAlliance) {
                    switch (color.kind) {
                        case BLUE:
                            RobotLog.i("163 First attempt, sensed blue");
                            beaconArms.deploy(true, true);
                            break;
                        case RED:
                            RobotLog.i("163 First attempt, sensed red");
                            beaconArms.deploy(false, true);
                            break;
                        case PURPLE:
                            RobotLog.i("163 Color sensor is not working");
                            beaconArms.stowServo();
                            break;
                    }
                } else {
                    switch (color.kind) {
                        case RED:
                            RobotLog.i("163 First attempt, sensed red");
                            beaconArms.deploy(true, true);
                            break;
                        case BLUE:
                            RobotLog.i("163 First attempt, sensed blue");
                            beaconArms.deploy(false, true);
                            break;
                        case PURPLE:
                            RobotLog.i("163 Color sensor is not working");
                            beaconArms.stowServo();
                            break;
                    }
                }
            }
        });
    }
}
