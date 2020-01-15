package opmodes.LM3;

import android.nfc.cardemulation.OffHostApduService;

import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import opmodes.calibration.MiyazakiCalibration;
import team25core.ColorSensorTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DistanceSensorCriteria;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankMechanumControlScheme;
import test.SkystoneDetectionTask;

import static opmodes.LM3.LisztSkybridgePath.ArmLocation.ARM_DEPLOYED;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "5218 Skystone Autonomous")
public class SternLM3Autonomous extends Robot {

    // drivetrain and mechanisms declaration
    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo leftStoneArm;
    private Servo rightStoneArm;
    private Servo purpleStoneArm;
    private Servo leftArm;
    private Servo rightArm;
    private MechanumGearedDrivetrain drivetrain;

    // sensor
    private ColorSensor leftColorSensor;
    private ColorSensor rightColorSensor;
    private ColorSensor purpleColorSensor;
    private DistanceSensor leftDistanceSensor;
    private DistanceSensor rightDistanceSensor;
    private DistanceSensor purpleDistanceSensor;
    private SkystoneDetectionTask skystoneDetectionTask;

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item stone;

    // skystone path declaration
    private SternSkystonePath skystone;
    private SternSkystonePath.AllianceColor allianceColor;
    private SternSkystonePath.ArmLocation stoneArms;
    private int MULTIPLIER = MiyazakiCalibration.ALLIANCE_MULTIPLIER;
    private int stoneOffset;
    private DeadReckonPath offsetStonePath;

    @Override
    public void init ()
    {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.dcMotor.get("frontLeft");
        frontRight = hardwareMap.dcMotor.get("frontRight");
        backLeft = hardwareMap.dcMotor.get("backLeft");
        backRight = hardwareMap.dcMotor.get("backRight");
        leftStoneArm = hardwareMap.servo.get("leftStoneArm");
        rightStoneArm = hardwareMap.servo.get("rightStoneArm");
        leftArm = hardwareMap.servo.get("leftArm");
        rightArm = hardwareMap.servo.get("rightArm");

        drivetrain = new MechanumGearedDrivetrain(60, frontRight, backRight, frontLeft, backLeft);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();

        // gamepad and telemetry initialization
        gamepad = new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);
        alliance = telemetry.addData("Alliance: ", "NOT SELECTED");
        stone = telemetry.addData("Stone Position: ", "NOT SELECTED");

        // sensor initialization
        leftColorSensor = hardwareMap.get(RevColorSensorV3.class, "leftColorSensor");
        rightColorSensor = hardwareMap.get(RevColorSensorV3.class, "rightColorSensor");
        leftDistanceSensor = hardwareMap.get(DistanceSensor.class, "leftColorSensor");
        rightDistanceSensor = hardwareMap.get(DistanceSensor.class, "rightColorSensor");

        // servo initialization
        stoneArms = SternSkystonePath.ArmLocation.ARM_STOWED;
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);

        // sky stone path initialization
        skystone = new SternSkystonePath();
        allianceColor = allianceColor.DEFAULT;
        DeadReckonPath offsetStonePath = new DeadReckonPath();
    }

    @Override
    public void handleEvent (RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;
            handleGamePadSelection(event);
        }
    }

    public void handleGamePadSelection (GamepadTask.GamepadEvent event)
    {
        switch (event.kind){
            case BUTTON_X_DOWN:
                allianceColor = allianceColor.BLUE;
                purpleColorSensor = rightColorSensor;
                purpleDistanceSensor = rightDistanceSensor;
                purpleStoneArm = rightStoneArm;
                alliance.setValue("BLUE");
                break;
            case BUTTON_B_DOWN:
                allianceColor = allianceColor.RED;
                MULTIPLIER = MULTIPLIER * -1;
                purpleColorSensor = leftColorSensor;
                purpleDistanceSensor = leftDistanceSensor;
                purpleStoneArm = leftStoneArm;
                alliance.setValue("RED");
                break;
        }
    }

    @Override
    public void start ()
    {
        initialMove();
    }

    public void initialMove ()
    {
        DeadReckonPath initialPath = new DeadReckonPath();
        initialPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 23, 0.6);
        addTask(new DeadReckonTask(this, initialPath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        drivetrain.stop();
                        findStoneEdge();
                        break;
                }
            }
        });
    }
    public void findStoneEdge()
    {
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 10);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, MULTIPLIER * .2);
        addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        drivetrain.stop();
                        drivetrain.resetEncoders();
                        if (purpleDistanceSensor.getDistance(DistanceUnit.CM) > 2) {
                            approachStone();
                        } else {
                            detectColor();
                        }
                        break;
                }
            }
        });
    }

    public void approachStone()
    {
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 2);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 100, .2);
        addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        drivetrain.stop();
                        detectColor();
                        break;
                }
            }
        });
    }

    public void detectColor()
    {
        drivetrain.resetEncoders();
        drivetrain.straight(0.4 * MULTIPLIER);
        skystoneDetectionTask = new SkystoneDetectionTask(this, purpleColorSensor, purpleDistanceSensor) {
            public void handleEvent(RobotEvent e) {
                SkystoneDetectionEvent event = (SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        stone.setValue("DETECTED");
                        drivetrain.stop();
                        moveStoneArms();
                        skystoneOffset();
                        break;
                }
            }
        };
        this.addTask(skystoneDetectionTask);
    }

    public void moveStoneArms ()
    {
        switch (stoneArms) {
            case ARM_DEPLOYED:
                switch(allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
                        stoneArms = SternSkystonePath.ArmLocation.ARM_STOWED;
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                        stoneArms = SternSkystonePath.ArmLocation.ARM_STOWED;
                        break;
                }
            case ARM_STOWED:
                switch(allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                        stoneArms = SternSkystonePath.ArmLocation.ARM_STOWED;
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                        stoneArms = SternSkystonePath.ArmLocation.ARM_STOWED;
                        break;
                }
                break;
        }
    }

    public void skystoneOffset()
    {
        stoneOffset = drivetrain.getCurrentPosition() / MiyazakiCalibration.ENCODERS_PER_INCH;
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset, MULTIPLIER * 0.4);
        runOffset(offsetStonePath);
    }

    public void runOffset(DeadReckonPath path)
    {
        int deliverDistance = stoneOffset + 60;
        DeadReckonPath deliverStone = new DeadReckonPath();
        deliverStone.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 14, -0.5);
        deliverStone.addSegment(DeadReckonPath.SegmentType.STRAIGHT, deliverDistance, MULTIPLIER * 0.5);
        addTask(new DeadReckonTask(this, path, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        moveStoneArms();
                        // approachSecondStone();
                        break;
                }
            }
        });
    }

    public void approachSecondStone()
    {

    }



}
