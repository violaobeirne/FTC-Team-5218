package opmodes.LM3;

import android.nfc.cardemulation.OffHostApduService;

import com.qualcomm.hardware.lynx.LynxModule;
import com.qualcomm.hardware.rev.RevColorSensorV3;
import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.ColorSensor;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorEx;
import com.qualcomm.robotcore.hardware.DistanceSensor;
import com.qualcomm.robotcore.hardware.Servo;
import com.qualcomm.robotcore.util.RobotLog;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.navigation.DistanceUnit;

import java.util.HashMap;
import java.util.List;

import opmodes.calibration.MiyazakiCalibration;
import team25core.ColorSensorTask;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.DistanceSensorCriteria;
import team25core.GamepadTask;
import team25core.MechanumGearedDrivetrain;
import team25core.MotorPackage;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.RobotTaskChain;
import team25core.SingleShotTimerTask;
import team25core.TankMechanumControlScheme;
import test.SkystoneDetectionTask;

import static opmodes.LM3.LisztSkybridgePath.ArmLocation.ARM_DEPLOYED;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "5218 Skystone Autonomous")
public class SternLM3Autonomous extends Robot {

    private final static String TAG = "SternLM3Autonomous";

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
    HashMap<MotorPackage.MotorLocation, MotorPackage> motorMap;

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

    private RobotTaskChain taskChain;

    @Override
    public void init ()
    {
        // drivetrain and mechanisms initialization
        frontLeft = hardwareMap.get(DcMotorEx.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotorEx.class, "frontRight");
        backLeft = hardwareMap.get(DcMotorEx.class, "backLeft");
        backRight = hardwareMap.get(DcMotorEx.class, "backRight");

        leftStoneArm = hardwareMap.get(Servo.class, "leftStoneArm");
        rightStoneArm = hardwareMap.get(Servo.class, "rightStoneArm");
        leftArm = hardwareMap.get(Servo.class, "leftArm");
        rightArm = hardwareMap.get(Servo.class, "rightArm");

        List<LynxModule> allHubs = hardwareMap.getAll(LynxModule.class);
        for (LynxModule module : allHubs) {
            module.setBulkCachingMode(LynxModule.BulkCachingMode.AUTO);
        }

        motorMap = new HashMap<>();
        motorMap.put(MotorPackage.MotorLocation.FRONT_LEFT, new MotorPackage(frontLeft));
        motorMap.put(MotorPackage.MotorLocation.FRONT_RIGHT, new MotorPackage(frontRight));
        motorMap.put(MotorPackage.MotorLocation.BACK_LEFT, new MotorPackage(backLeft));
        motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight, 0.815, MotorPackage.OffsetPolarity.POLARITY_POSITIVE));

        drivetrain = new MechanumGearedDrivetrain(motorMap);
        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setNoncanonicalMotorDirection();
        drivetrain.setMasterMotor(frontLeft);

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

        taskChain = new RobotTaskChain(this);
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
        /*
         * Sets up a robot task chain.  All operations here will run serialized.
         */
        initialMove();
        findStoneEdge();
        detectSkyStone();
        doWait(1000);
        deliverFirstStone();
        moveToSecondStone();
        deliverSecondStone();

        /*
         * Start the task chain.
         */
        addTask(taskChain);
    }

    public void deliverFirstStone()
    {
        RobotLog.i(TAG, "deliverStone");
        DeadReckonPath deliverPath = new DeadReckonPath();
        deliverPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        deliverPath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, -0.5);
        taskChain.addTask(new DeadReckonTask(this, deliverPath, drivetrain));
    }

    public void moveToSecondStone()
    {
    }

    public void deliverSecondStone()
    {
    }

    public void doWait(int ms)
    {
        RobotLog.i(TAG, "doWait");
        taskChain.addTask(new SingleShotTimerTask(this, ms));
    }

    public void initialMove ()
    {
        RobotLog.i(TAG, "initialMove");
        DeadReckonPath initialPath = new DeadReckonPath();
        initialPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 36, 0.5);

        taskChain.addTask(new DeadReckonTask(this, initialPath, drivetrain));
    }

    public void findStoneEdge()
    {
        RobotLog.i(TAG, "findStoneEdge");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 5);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, MULTIPLIER * -.2);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.i(TAG, "Found stone edge");
                        this.disableSensors();
                        robot.removeTask(this);
                        break;
                }
            }
        });
    }

    public void approachStone()
    {
        RobotLog.i(TAG, "approachStone");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 2);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 100, .2);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        drivetrain.stop();
                        robot.removeTask(this);
                        break;
                }
            }
        });
    }

    public void detectSkyStone()
    {
        RobotLog.i(TAG, "detectSkystone");
        drivetrain.resetEncoders();
        drivetrain.straight(0.4 * MULTIPLIER);
        taskChain.addTask(new SkystoneDetectionTask(this, purpleColorSensor, purpleDistanceSensor) {
            public void handleEvent(RobotEvent e) {
                SkystoneDetectionEvent event = (SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        RobotLog.i(TAG, "Color detected");
                        stone.setValue("DETECTED");
                        stoneOffset = drivetrain.getCurrentPosition();
                        drivetrain.stop();
                        moveStoneArms();
                        break;
                }
            }
        });
    }

    public void moveBack()
    {
        RobotLog.i("163 moveBack");
        DeadReckonPath moveToStone;
        moveToStone = new DeadReckonPath();
        moveToStone.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10.0, MULTIPLIER * 0.4);
        taskChain.addTask(new DeadReckonTask(this, moveToStone, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch(event.kind) {
                    case PATH_DONE:
                        stoneOffset += drivetrain.getCurrentPosition();
                        break;
                }
            }
        });
    }

    public void moveStoneArms ()
    {
        RobotLog.i(TAG, "moveStoneArms");
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

    public void pullBack()
    {
        RobotLog.i(TAG, "pullBack");
        DeadReckonPath pullback = new DeadReckonPath();
        pullback.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 6, -0.5);
        addTask(new DeadReckonTask(this, pullback, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;

            }
        });
    }


    public void skystoneOffset() //
    {
        RobotLog.i(TAG, "skystoneOffset");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        stoneOffset = drivetrain.getCurrentPosition() / MiyazakiCalibration.ENCODERS_PER_INCH;
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset, MULTIPLIER * 0.4);
        runOffset(offsetStonePath);
    }

    public void runOffset(DeadReckonPath path)
    {
        RobotLog.i("163 runOffset");
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
        RobotLog.i("163 approachSecondStone");
    }



}
