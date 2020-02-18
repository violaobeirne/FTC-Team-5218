package opmodes.ILT;

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

import java.util.HashMap;
import java.util.List;

import opmodes.calibration.MiyazakiCalibration;
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
import test.SkystoneDetectionTask;

/**
 * Created by Lizzie on 1/6/2020.
 */

@Autonomous(name = "KINDA BROKEN 5218 ILT Skystone Autonomous")
public class BeethovenILTAutonomous extends Robot {

    private final static String TAG = "SternILTAutonomous";

    public enum ArmToggle {
        STOW_ARM,
        DEPLOY_ARM
    }

    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT,
    }

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

    // gamepad and telemetry declaration
    private GamepadTask gamepad;
    private Telemetry.Item alliance;
    private Telemetry.Item stone;

    // autonomous variable declaration
    private AllianceColor allianceColor;
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
        motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight));
        //motorMap.put(MotorPackage.MotorLocation.BACK_RIGHT, new MotorPackage(backRight, 0.815, MotorPackage.OffsetPolarity.POLARITY_POSITIVE));

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
        leftArm.setPosition(MiyazakiCalibration.ARM_LEFT_STOW);
        rightArm.setPosition(MiyazakiCalibration.ARM_RIGHT_STOW);
        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);

        // sky stone path initialization
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
        doWait(1000);
        findStoneEdge();
        doWait(1000);
        detectSkyStone();
        doWait(1000);
        approachStone();
        doWait(1000);
        pullStoneBack();
        doWait(1000);
        deliverFirstStone();
        doWait(1000);
        moveToSecondStone();
        doWait(1000);
        approachSecondStone();
        doWait(1000);
        pullSecondStoneBack();
        doWait(1000);
        deliverSecondStone();
        /*
         * Start the task chain.
         */
        addTask(taskChain);
    }

    public void doWait(int ms)
    {
        RobotLog.ii(TAG, "Setup doWait");
        taskChain.addTask(new SingleShotTimerTask(this, ms));
    }

    public void initialMove () // 1
    {
        // taskChain.addTask(new CentrifugeTask(this, drivetrain, 33, 0.8, false));
        RobotLog.ii(TAG, "Setup initialMove");
        DeadReckonPath initialPath;
        initialPath = new DeadReckonPath();
        initialPath.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, 0.4);
        taskChain.addTask(new DeadReckonTask(this, initialPath, drivetrain));
    }

    public void findStoneEdge() // 2
    {
        RobotLog.ii(TAG, "Setup findStoneEdge");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 5);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, MULTIPLIER * -.1);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.ii(TAG, "Found stone edge");
                        this.disableSensors();
                        robot.removeTask(this);
                        drivetrain.resetEncoders();
                        drivetrain.encodersOn();
                        drivetrain.straight(-0.2 * MULTIPLIER);
                        break;
                }
            }
        });
    }

    public void detectSkyStone() // 3
    {
        RobotLog.ii(TAG, "Setup detectSkystone");
        taskChain.addTask(new SkystoneDetectionTask(this, purpleColorSensor, purpleDistanceSensor) {
            public void handleEvent(RobotEvent e) {
                SkystoneDetectionEvent event = (SkystoneDetectionEvent) e;
                switch (event.kind) {
                    case STONE_DETECTED:
                        RobotLog.ii(TAG, "Color detected");
                        stone.setValue("DETECTED");
                        stoneOffset = drivetrain.getCurrentPosition() / MiyazakiCalibration.ENCODERS_PER_INCH;
                        drivetrain.stop();
                        break;
                }
            }
        });
    }

    public void approachStone() // 4
    {
        RobotLog.ii(TAG, "Setup approachStone");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 2);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 100, .2);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.ii(TAG, "Finished approaching stone");
                        drivetrain.stop();
                        robot.removeTask(this);
                        moveStoneArms(ArmToggle.DEPLOY_ARM);
                        break;
                }
            }
        });
    }

    public void pullStoneBack() // 5, 9
    {
        RobotLog.ii(TAG, "Setup pullStoneBack");
        DeadReckonPath pullBack = new DeadReckonPath();
        pullBack.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 15, -0.4);
        taskChain.addTask(new DeadReckonTask(this, pullBack, drivetrain));
    }

    public void deliverFirstStone () // 6
    {
        RobotLog.ii(TAG, "Setup skystoneOffset");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset + 45, MULTIPLIER * 0.8);
        taskChain.addTask(new DeadReckonTask(this, offsetStonePath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        moveStoneArms(ArmToggle.STOW_ARM);
                        RobotLog.ii(TAG, "Finshed delivering first stone.");
                        break;
                }
            }
        });
    }

    public void moveToSecondStone() // 7
    {
        RobotLog.ii(TAG, "Setup moveToSecondStone");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset + 45 + 21, -MULTIPLIER * 0.8);
        taskChain.addTask(new DeadReckonTask(this, offsetStonePath, drivetrain));
    }

    public void approachSecondStone() // 8
    {
        RobotLog.ii(TAG, "Setup approachSecondStone");
        DistanceSensorCriteria dsc = new DistanceSensorCriteria(purpleDistanceSensor, 2);
        DeadReckonPath distanceOffset = new DeadReckonPath();
        distanceOffset.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 100, .2);
        taskChain.addTask(new DeadReckonTask(this, distanceOffset, drivetrain, dsc) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case SENSOR_SATISFIED:
                        RobotLog.ii(TAG, "Finished approaching second stone");
                        drivetrain.stop();
                        robot.removeTask(this);
                        moveStoneArms(ArmToggle.DEPLOY_ARM);
                        break;
                }
            }
        });
    }

    public void pullSecondStoneBack () // 9
    {
        RobotLog.ii(TAG, "Setup pullSecondStoneBack");
        DeadReckonPath pullBack = new DeadReckonPath();
        pullBack.addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 15, -0.4);
        taskChain.addTask(new DeadReckonTask(this, pullBack, drivetrain));
    }

    public void deliverSecondStone() // 10
    {
        RobotLog.ii(TAG, "Setup deliverSecondStone");
        DeadReckonPath offsetStonePath = new DeadReckonPath();
        offsetStonePath.addSegment(DeadReckonPath.SegmentType.STRAIGHT, stoneOffset + 45 + 35, MULTIPLIER * 0.8);
        taskChain.addTask(new DeadReckonTask(this, offsetStonePath, drivetrain) {
            public void handleEvent(RobotEvent e) {
                DeadReckonEvent event = (DeadReckonEvent) e;
                switch (event.kind) {
                    case PATH_DONE:
                        RobotLog.ii(TAG, "Finshed delivering second stone.");
                        moveStoneArms(ArmToggle.STOW_ARM);
                        break;
                }
            }
        });
    }

    public void moveStoneArms(ArmToggle arm)
    {
        RobotLog.ii(TAG, "moveStoneArms");
        switch(arm) {
            case DEPLOY_ARM:
                switch (allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_DOWN);
                        RobotLog.ii(TAG, "Deployed right stone arm.");
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_DOWN);
                        RobotLog.ii(TAG, "Deployed left stone arm.");
                        break;
                }
                break;

            case STOW_ARM:
                switch (allianceColor) {
                    case BLUE:
                        rightStoneArm.setPosition(MiyazakiCalibration.STONE_RIGHT_ARM_STOW);
                        RobotLog.ii(TAG, "Stowed right stone arm.");
                        break;
                    case RED:
                        leftStoneArm.setPosition(MiyazakiCalibration.STONE_LEFT_ARM_STOW);
                        RobotLog.ii(TAG, "Stowed left stone arm.");
                        break;
                }
                break;
        }
    }

    /*
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
     */
}
